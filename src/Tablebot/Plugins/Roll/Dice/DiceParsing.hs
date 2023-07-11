{-# LANGUAGE LiberalTypeSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Tablebot.Plugins.Roll.Dice.DiceParsing
-- Description : Parsers for parsing dice and other expressions.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This plugin contains the tools for parsing Dice. -Wno-orphans is enabled so
-- that parsing can occur here instead of in SmartParser or DiceData.
module Tablebot.Plugins.Roll.Dice.DiceParsing () where

import Data.Functor (($>), (<&>))
import Data.List (sortBy)
import Data.List.NonEmpty as NE (fromList)
import Data.Map as M (Map, findWithDefault, keys, map, (!))
import Data.Set as S (Set, fromList, map)
import qualified Data.Text as T
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceFunctions
  ( ArgType (..),
    FuncInfoBase (..),
    integerFunctions,
    listFunctions,
  )
import Tablebot.Utility.Parser
import Tablebot.Utility.SmartParser (CanParse (..), (<??>))
import Tablebot.Utility.Types (Parser)
import Text.Megaparsec (MonadParsec (try), choice, failure, many, optional, some, (<?>), (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Error (ErrorItem (Tokens))

-- | An easier way to handle failure in parsers.
failure' :: T.Text -> Set T.Text -> Parser a
failure' s ss = failure (Just $ Tokens $ NE.fromList $ T.unpack s) (S.map (Tokens . NE.fromList . T.unpack) ss)

variableName :: Parser T.Text
variableName = T.pack <$> some (choice $ char <$> '_' : ['a' .. 'z'])

instance CanParse a => CanParse (Var a) where
  pars = do
    _ <- try (string "var") <* skipSpace
    letCon <- try (char '!' $> VarLazy) <|> return Var
    varName' <- variableName
    _ <- skipSpace >> char '=' >> skipSpace
    letCon varName' <$> pars

instance CanParse Statement where
  pars = ((StatementListValues <$> try pars) <|> (StatementExpr <$> pars)) <* skipSpace <* char ';' <* skipSpace

{-
-- alternative method to the above.
-- from https://canary.discord.com/channels/280033776820813825/280036215477239809/938154455612919838
-- - Morrow#1157
newtype VarCon = VarCon (forall a. a -> Var a)

parseLet :: Parser VarCon
parseLet = do
  _ <- try (string "var") <* skipSpace
  lazy <- try (char '!' $> True) <|> return False
  varName' <- varName
  _ <- skipSpace >> char '=' >> skipSpace
  return $ VarCon (\a -> if lazy then VarLazy varName' a else Var varName' a)

instance CanParse Statement where
  pars = do
    VarCon letP <- parseVar
    val <- (Left <$> pars <|> Right <$> pars) <* skipSpace <* char ';' <* skipSpace
    return $ either (VarList . letP) (VarExpr . letP) val
-}

parseStatements :: Parser [Statement]
parseStatements = do
  s <- optional $ try pars
  case s of
    Nothing -> return []
    Just s' -> (s' :) <$> parseStatements

instance CanParse Program where
  pars = parseStatements >>= \ss -> Program ss <$> pars

instance CanParse ListValues where
  pars =
    do
      functionParser listFunctions LVFunc
      <|> (LVVar . ("l_" <>) <$> try (string "l_" *> variableName))
      <|> ListValuesMisc <$> (pars >>= checkVar)
      <|> (try (pars <* char '#') >>= \nb -> MultipleValues nb <$> pars)
      <|> LVBase <$> pars
    where
      checkVar (MiscVar l)
        | T.isPrefixOf "l_" (varName l) = return (MiscVar l)
        | otherwise = fail "list variables must be prepended with l_"
      checkVar l = return l

instance CanParse ListValuesBase where
  pars = do
    LVBList
      <$> ( try (char '{' *> skipSpace)
              *> parseCommaSeparated1 pars
              <* skipSpace
              <* (char '}' <??> "could not find closing brace for list")
          )
      <|> LVBParen . unnest
      <$> pars
    where
      unnest (Paren (LVBase (LVBParen e))) = e
      unnest e = e

-- | Helper function to try to parse the second part of a binary operator.
binOpParseHelp :: (CanParse a) => Char -> (a -> a) -> Parser a
binOpParseHelp c con = try (skipSpace *> char c) *> skipSpace *> (con <$> pars)

instance (CanParse b) => CanParse (If b) where
  pars = do
    a <- string "if" *> skipSpace1 *> pars <* skipSpace1
    t <- string "then" *> skipSpace1 *> pars <* skipSpace1
    e <- string "else" *> skipSpace1 *> pars
    return $ If a t e

instance CanParse a => CanParse (MiscData a) where
  pars = (MiscVar <$> pars) <|> (MiscIf <$> pars)

instance (CanParse sub, CanParse typ, Operation typ) => CanParse (BinOp sub typ) where
  pars = do
    a <- pars
    tas <- many parseTas
    return $ BinOp a tas
    where
      parseTas = try $ do
        t <- skipSpace *> pars
        a' <- skipSpace *> pars
        return (t, a')

instance CanParse ExprType where
  pars = try (char '+' $> Add) <|> try (char '-' $> Sub)

instance CanParse Expr where
  pars =
    (ExprMisc <$> pars) <|> (Expr <$> pars)

instance CanParse TermType where
  pars = try (char '*' $> Multi) <|> try (char '/' $> Div)

instance CanParse Term where
  pars = Term <$> pars

instance CanParse Func where
  pars = functionParser integerFunctions Func <|> NoFunc <$> pars

-- | A generic function parser that takes a mapping from function names to
-- functions, the main way to contruct the function data type `e`, and a
-- constructor for `e` that takes only one value, `a` (which has its own,
-- previously defined parser).
functionParser :: M.Map T.Text (FuncInfoBase j) -> (FuncInfoBase j -> [ArgValue] -> e) -> Parser e
functionParser m mainCons =
  do
    fi <- try (choice (string <$> functionNames) >>= \t -> return (m M.! t)) <?> "could not find function"
    let ft = funcInfoParameters fi
    es <- skipSpace *> string "(" *> skipSpace *> parseArgValues ft <* skipSpace <* (string ")" <??> "could not find closing bracket on function call")
    return $ mainCons fi es
  where
    functionNames = sortBy (\a b -> compare (T.length b) (T.length a)) $ M.keys m

instance CanParse Negation where
  pars =
    try (char '-') *> skipSpace *> (Neg <$> pars)
      <|> NoNeg <$> pars

instance CanParse Expo where
  pars = do
    t <- pars
    binOpParseHelp '^' (Expo t) <|> (return . NoExpo) t

instance CanParse NumBase where
  pars =
    (NBParen . unnest <$> pars)
      <|> Value <$> integer <??> "could not parse integer"
    where
      unnest (Paren (Expr (SingBinOp (Term (SingBinOp (NoNeg (NoExpo (NoFunc (NBase (NBParen e)))))))))) = e
      unnest e = e

instance (CanParse a) => CanParse (Paren a) where
  pars = try (char '(') *> skipSpace *> (Paren <$> pars) <* skipSpace <* char ')'

instance CanParse Base where
  pars =
    ( do
        nb <- try pars <?> "could not parse numbase in base"
        (DiceBase <$> parseDice nb)
          <|> return (NBase nb)
    )
      <|> DiceBase <$> parseDice (Value 1)
      <|> (NumVar <$> try variableName)

instance CanParse Die where
  pars = do
    _ <- try (char 'd') <?> "could not find 'd' for die"
    lazyFunc <- (try (char '!') $> LazyDie) <|> return id
    lazyFunc
      <$> ( (CustomDie . LVBParen <$> try pars <|> Die . NBParen <$> pars)
              <|> ( (CustomDie <$> pars <??> "could not parse list values for die")
                      <|> (Die <$> pars <??> "could not parse base number for die")
                  )
          )

-- | Given a `NumBase` (the value on the front of a set of dice), construct a
-- set of dice.
parseDice :: NumBase -> Parser Dice
parseDice nb = parseDice' <*> return (NBase nb)

-- | Helper for parsing Dice, where as many `Dice` as possible are parsed and a
-- function that takes a `Base` value and returns a `Dice` value is returned.
-- This `Base` value is meant to be first value that `Dice` have.
parseDice' :: Parser (Base -> Dice)
parseDice' = do
  d <- (pars :: Parser Die)
  mdor <- parseDieOpRecur

  ( do
      bd <- try parseDice' <?> "trying to recurse dice failed"
      return (\b -> bd (DiceBase $ Dice b d mdor))
    )
    <|> return (\b -> Dice b d mdor)

-- | Parse a `/=`, `<=`, `>=`, `<`, `=`, `>` as an `AdvancedOrdering`.
parseAdvancedOrdering :: Parser AdvancedOrdering
parseAdvancedOrdering = (try (choice opts) <?> "could not parse an ordering") >>= matchO
  where
    matchO s = M.findWithDefault (failure' s (S.fromList opts')) s (M.map return $ fst advancedOrderingMapping)
    opts' = sortBy (\a b -> compare (T.length b) (T.length a)) $ M.keys $ fst advancedOrderingMapping
    opts = fmap string opts'

-- | Parse a `LowHighWhere`, which is an `h` followed by an integer.
parseLowHigh :: Parser LowHighWhere
parseLowHigh = ((choice @[] $ char <$> "lhw") <??> "could not parse high, low or where") >>= helper
  where
    helper 'h' = High <$> pars
    helper 'l' = Low <$> pars
    helper 'w' = parseAdvancedOrdering >>= \o -> pars <&> Where o
    helper c = failure' (T.singleton c) (S.fromList ["h", "l", "w"])

-- | Parse a bunch of die options into, possibly, a DieOpRecur.
parseDieOpRecur :: Parser (Maybe DieOpRecur)
parseDieOpRecur = do
  dopo <- optional parseDieOpOption
  maybe (return Nothing) (\dopo' -> Just . DieOpRecur dopo' <$> parseDieOpRecur) dopo

-- | Parse a single die option.
parseDieOpOption :: Parser DieOpOption
parseDieOpOption = do
  lazyFunc <- (try (char '!') $> DieOpOptionLazy) <|> return id
  ( ( (try (string "ro") *> parseAdvancedOrdering >>= \o -> Reroll True o <$> pars)
        <|> (try (string "rr") *> parseAdvancedOrdering >>= \o -> Reroll False o <$> pars)
        <|> ( ( ((try (char 'k') *> parseLowHigh) <&> DieOpOptionKD Keep)
                  <|> ((try (char 'd') *> parseLowHigh) <&> DieOpOptionKD Drop)
              )
                <?> "could not parse keep/drop"
            )
    )
      <&> lazyFunc
    )
    <?> "could not parse dieOpOption - expecting one of the options described in the doc (call `help roll` to access)"

-- | Parse a single `ArgType` into an `ArgValue`.
parseArgValue :: ArgType -> Parser ArgValue
parseArgValue ATIntegerList = AVListValues <$> pars <?> "could not parse a list value from the argument"
parseArgValue ATInteger = AVExpr <$> pars <?> "could not parse an integer from the argument"

-- | Parse a list of comma separated arguments.
parseArgValues :: [ArgType] -> Parser [ArgValue]
parseArgValues [] = return []
parseArgValues [at] = (: []) <$> parseArgValue at
parseArgValues (at : ats) = parseArgValue at >>= \av -> skipSpace *> (try (char ',') <?> "expected " ++ show (length ats) ++ " more arguments") *> skipSpace *> ((av :) <$> parseArgValues ats)

--- Pretty printing the AST

instance ParseShow ArgValue where
  parseShow (AVExpr e) = parseShow e
  parseShow (AVListValues lv) = parseShow lv

instance ParseShow ListValues where
  parseShow (LVBase e) = parseShow e
  parseShow (MultipleValues nb b) = parseShow nb <> "#" <> parseShow b
  parseShow (LVFunc s n) = funcInfoName s <> "(" <> T.intercalate ", " (parseShow <$> n) <> ")"
  parseShow (LVVar t) = t
  parseShow (ListValuesMisc l) = parseShow l

instance ParseShow ListValuesBase where
  parseShow (LVBList es) = "{" <> T.intercalate ", " (parseShow <$> es) <> "}"
  parseShow (LVBParen p) = parseShow p

instance ParseShow a => ParseShow (MiscData a) where
  parseShow (MiscVar l) = parseShow l
  parseShow (MiscIf l) = parseShow l

instance (ParseShow sub, ParseShow typ) => ParseShow (BinOp sub typ) where
  parseShow (BinOp a tas) = parseShow a <> T.concat (fmap (\(t, a') -> " " <> parseShow t <> " " <> parseShow a') tas)

instance ParseShow ExprType where
  parseShow Add = "+"
  parseShow Sub = "-"

instance ParseShow TermType where
  parseShow Multi = "*"
  parseShow Div = "/"

instance ParseShow Expr where
  parseShow (Expr e) = parseShow e
  parseShow (ExprMisc e) = parseShow e

instance ParseShow Term where
  parseShow (Term f) = parseShow f

instance ParseShow Func where
  parseShow (Func s n) = funcInfoName s <> "(" <> T.intercalate ", " (parseShow <$> n) <> ")"
  parseShow (NoFunc b) = parseShow b

instance ParseShow Negation where
  parseShow (Neg expo) = "-" <> parseShow expo
  parseShow (NoNeg expo) = parseShow expo

instance ParseShow Expo where
  parseShow (NoExpo b) = parseShow b
  parseShow (Expo b expo) = parseShow b <> " ^ " <> parseShow expo

instance ParseShow NumBase where
  parseShow (NBParen p) = parseShow p
  parseShow (Value i) = T.pack $ show i

instance (ParseShow a) => ParseShow (Paren a) where
  parseShow (Paren a) = "(" <> parseShow a <> ")"

instance ParseShow Base where
  parseShow (NBase nb) = parseShow nb
  parseShow (DiceBase dop) = parseShow dop
  parseShow (NumVar t) = t

instance ParseShow Die where
  parseShow (Die b) = "d" <> parseShow b
  parseShow (CustomDie lv) = "d" <> parseShow lv
  -- parseShow (CustomDie is) = "d{" <> intercalate ", " (parseShow <$> is) <> "}"
  parseShow (LazyDie d) = "d!" <> T.tail (parseShow d)

instance ParseShow Dice where
  parseShow (Dice b d dor) = parseShow b <> parseShow d <> helper' dor
    where
      fromOrdering ao = M.findWithDefault "??" ao $ snd advancedOrderingMapping
      fromLHW (Where o i) = "w" <> fromOrdering o <> parseShow i
      fromLHW (Low i) = "l" <> parseShow i
      fromLHW (High i) = "h" <> parseShow i
      helper' Nothing = ""
      helper' (Just (DieOpRecur dopo' dor')) = helper dopo' <> helper' dor'
      helper (DieOpOptionLazy doo) = "!" <> helper doo
      helper (Reroll True o i) = "ro" <> fromOrdering o <> parseShow i
      helper (Reroll False o i) = "rr" <> fromOrdering o <> parseShow i
      helper (DieOpOptionKD Keep lhw) = "k" <> fromLHW lhw
      helper (DieOpOptionKD Drop lhw) = "d" <> fromLHW lhw

instance (ParseShow a) => ParseShow (Var a) where
  parseShow (Var t a) = "var " <> t <> " = " <> parseShow a
  parseShow (VarLazy t a) = "var !" <> t <> " = " <> parseShow a

instance (ParseShow b) => ParseShow (If b) where
  parseShow (If b t e) = "if " <> parseShow b <> " then " <> parseShow t <> " else " <> parseShow e

instance ParseShow Statement where
  parseShow (StatementExpr l) = parseShow l <> "; "
  parseShow (StatementListValues l) = parseShow l <> "; "

instance ParseShow Program where
  parseShow (Program ss a) = foldr ((<>) . parseShow) (parseShow a) ss
