-- |
-- Module      : Tablebot.Plugins.Roll.Plugin
-- Description : A command that outputs the result of rolling dice.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that outputs the result of rolling the input dice.
module Tablebot.Plugins.Roll.Plugin (rollPlugin) where

import Control.Monad.Writer (MonadIO (liftIO), void)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default (def))
import Data.Distribution (isValid)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, intercalate, pack, replicate, unpack)
import qualified Data.Text as T
import Discord (restCall)
import Discord.Interactions
  ( Interaction (..),
  )
import Discord.Internal.Rest.Channel (ChannelRequest (..), MessageDetailedOpts (..))
import Discord.Types (ActionRow (..), Button (..), Message (..), User (..), UserId, mkButton, mkEmoji)
import System.Timeout (timeout)
import Tablebot.Internal.Handler.Command (parseValue)
import Tablebot.Plugins.Roll.Dice
import Tablebot.Plugins.Roll.Dice.DiceData
import Tablebot.Plugins.Roll.Dice.DiceStats (getStats, rangeExpr)
import Tablebot.Plugins.Roll.Dice.DiceStatsBase (distributionByteString)
import Tablebot.Utility
import Tablebot.Utility.Discord (Format (Code), formatText, sendCustomMessage, sendMessage, toMention')
import Tablebot.Utility.Exception (BotException (EvaluationException), throwBot)
import Tablebot.Utility.Parser
import Tablebot.Utility.SmartParser
import Text.Megaparsec
import Text.RawString.QQ (r)

-- | The basic execution function for rolling dice. Both the expression and message are
-- optional. If the expression is not given, then the default roll is used.
-- The userid of the user that called this command is also given.
rollDice'' :: Maybe Program -> Maybe (Quoted Text) -> UserId -> DatabaseDiscord Text
rollDice'' e' t uid = do
  let e = fromMaybe (Program [] (Right defaultRoll)) e'
  maybemsss <- liftIO $ timeout 1000000 $ evalProgram e
  case maybemsss of
    Nothing -> throwBot (EvaluationException "Could not process expression in one second" [])
    -- vs is either a list of integers and their textual representation, or
    -- a single integer. ss is the message
    Just (vs, ss) -> do
      let msg = makeMsg vs ss
      if countFormatting msg < 199
        then return msg
        else return (makeMsg (simplify vs) (parseShow e <> " `[could not display rolls]`"))
  where
    dsc = maybe ": " (\(Qu t') -> " \"" <> t' <> "\": ") t
    baseMsg = toMention' uid <> " rolled" <> dsc
    makeLine (i, s) = pack (show i) <> Data.Text.replicate (max 0 (6 - length (show i))) " " <> " ⟵ " <> s
    makeMsg (Right v) s = baseMsg <> s <> ".\nOutput: " <> pack (show v)
    makeMsg (Left []) _ = baseMsg <> "No output."
    makeMsg (Left ls) ss
      | all (T.null . snd) ls = baseMsg <> ss <> "\nOutput: {" <> intercalate ", " (pack . show . fst <$> ls) <> "}"
      | otherwise = baseMsg <> ss <> "\n  " <> intercalate "\n  " (makeLine <$> ls)
    simplify (Left ls) = Left $ fmap (\(i, _) -> (i, "...")) ls
    simplify li = li
    countFormatting s = (`div` 4) $ T.foldr (\c cf -> cf + (2 * fromEnum (c == '`')) + fromEnum (c `elem` ['~', '_', '*'])) 0 s

-- | A version of rollDice'' that is nicer to parse and has a constructed message.
rollDice' :: Maybe Program -> Maybe (Quoted Text) -> SenderUserId -> DatabaseDiscord MessageDetails
rollDice' e t (SenderUserId uid) = do
  msg <- rollDice'' e t uid
  return
    ( (messageDetailsBasic msg)
        { messageDetailsComponents =
            Just
              [ ActionRowButtons
                  -- we take the first 100 characters of the button customid
                  --  because they're only allowed to be 100 characters long.
                  -- the button is disabled if it's meant to be more than 100
                  --  characters so we don't have to worry about this.
                  [ (mkButton buttonName (T.take 100 buttonCustomId))
                      { buttonEmoji = Just (mkEmoji "🎲"),
                        buttonDisabled = buttonDisabled
                      }
                  ]
              ]
        }
    )
  where
    appendIf t' Nothing = t'
    appendIf t' (Just e') = t' <> " " <> parseShow e'
    buttonCustomId = (("roll reroll " <> pack (show uid)) `appendIf` e) `appendIf` t
    (buttonName, buttonDisabled) = if T.length buttonCustomId > 100 then ("Expr too long", True) else ("Reroll", False)

rollSlashCommandFunction :: Labelled "expression" "what's being evaluated" (Maybe Text) -> Labelled "quote" "associated message" (Maybe (Quoted Text)) -> SenderUserId -> DatabaseDiscord MessageDetails
rollSlashCommandFunction (Labelled mt) (Labelled qt) suid = do
  lve <- mapM (parseValue (pars <* eof)) mt
  rollDice' lve qt suid

rerollComponentRecv :: ComponentRecv
rerollComponentRecv = ComponentRecv "reroll" (processComponentInteraction' rollDiceParserI True)

-- | Manually creating parser for this command, since SmartCommand doesn't work fully for
-- multiple Maybe values
rollDiceParser :: Parser (Message -> DatabaseDiscord ())
rollDiceParser = choice (try <$> options)
  where
    -- Just the value is given to the command, no quote.
    justEither :: WithError "Incorrect expression/list value. Please check the expression" Program -> SenderUserId -> DatabaseDiscord MessageDetails
    justEither (WErr x) = rollDice' (Just x) Nothing
    -- Nothing is given to the command, a default case.
    nothingAtAll :: WithError "Expected eof" () -> SenderUserId -> DatabaseDiscord MessageDetails
    nothingAtAll (WErr _) = rollDice' Nothing Nothing
    -- Both the value and the quote are present.
    bothVals :: WithError "Incorrect format. Please check the expression and quote" (Program, Quoted Text) -> SenderUserId -> DatabaseDiscord MessageDetails
    bothVals (WErr (x, y)) = rollDice' (Just x) (Just y)
    -- Just the quote is given to the command.
    justText :: WithError "Incorrect quote. Please check the quote format" (Quoted Text) -> SenderUserId -> DatabaseDiscord MessageDetails
    justText (WErr x) = rollDice' Nothing (Just x)
    options =
      [ parseComm justEither,
        parseComm nothingAtAll,
        parseComm bothVals,
        parseComm justText
      ]

-- | Creating a parser for the component interactions stuff. Needs to be
-- manually made since I think the maybe parser stuff doesn't work properly
-- still?
rollDiceParserI :: Parser (Interaction -> DatabaseDiscord MessageDetails)
rollDiceParserI = choice (try <$> options)
  where
    options =
      [ onlyAllowRequestor (\lv -> rollDice' (Just lv) Nothing),
        onlyAllowRequestor (rollDice' Nothing Nothing),
        onlyAllowRequestor (\lv qt -> rollDice' (Just lv) (Just qt)),
        onlyAllowRequestor (rollDice' Nothing . Just)
      ]

-- | Basic command for rolling dice.
rollDice :: Command
rollDice = Command "roll" rollDiceParser [statsCommand]

-- where
--   rollDiceParser = parseComm rollDiceParser'
--   rollDiceParser' :: WithError "Incorrect rolling format. Please check your expression and quote is of the correct format" (Maybe (Either ListValues Expr), Maybe (Quoted Text)) -> Message -> DatabaseDiscord ()
--   rollDiceParser' (WErr (x, y)) = rollDice' x y

-- | Rolling dice inline.
rollDiceInline :: InlineCommand
rollDiceInline = inlineCommandHelper "[|" "|]" pars (\e m -> runFunc e m >>= sendCustomMessage m)
  where
    runFunc e m = rollDice' (Just e) Nothing (SenderUserId $ userId $ messageAuthor m)

-- | Help page for rolling dice, with a link to the help page.
rollHelp :: HelpPage
rollHelp =
  HelpPage
    "roll"
    ["r"]
    "roll dice and do maths"
    rollHelpText
    [statsHelp]
    None

-- | A large chunk of help text for the roll command.
rollHelpText :: Text
rollHelpText =
  pack $
    [r|**Roll**
Given an expression, evaluate the expression. Can roll inline using |]
      ++ "`[|to roll|]`."
      ++ [r| Can use `r` instead of `roll`.

This supports addition, subtraction, multiplication, integer division, exponentiation, parentheses, rolling dice of arbitrary size (up to |]
      ++ show maximumRNG
      ++ [r| RNG calls), dice with custom sides, rerolling dice once on a condition, rerolling dice indefinitely on a condition, keeping or dropping the highest or lowest dice, keeping or dropping dice based on a condition, operating on lists (which have a maximum size of |]
      ++ show maximumListLength
      ++ [r|), if statements, var statements, and using functions like |]
      ++ unpack (intercalate ", " integerFunctionsList)
      ++ [r| (which return integers), or functions like |]
      ++ unpack (intercalate ", " listFunctionsList)
      ++ [r| (which return lists).

To see a full list of uses, options and limitations, please go to <https://github.com/WarwickTabletop/tablebot/blob/stable/docs/Roll.md>.

*Usage:*
  - `roll 1d20` -> rolls a twenty sided die and returns the outcome
  - `roll 3d6 + 5d4` -> sums the result of rolling three d6s and five d4s
  - `roll 2d20kh1` -> keeps the highest value out of rolling two d20s
  - `roll 5d10dl4` -> roll five d10s and drop the lowest four
|]

-- | Command for generating characters.
genchar :: Command
genchar = Command "genchar" (snd $ head rpgSystems') (toCommand <$> rpgSystems')
  where
    doDiceRoll (nm, lv) = (nm, parseComm $ rollDice' (Just (Program [] (Left lv))) (Just (Qu ("genchar for " <> nm))))
    rpgSystems' = doDiceRoll <$> rpgSystems
    toCommand (nm, ps) = Command nm ps []

-- | List of supported genchar systems and the dice used to roll for them
rpgSystems :: [(Text, ListValues)]
rpgSystems =
  [ ("dnd", MultipleValues (Value 6) (DiceBase (Dice (NBase (Value 4)) (Die (Value 6)) (Just (DieOpRecur (DieOpOptionKD Drop (Low (Value 1))) Nothing))))),
    ("wfrp", MultipleValues (Value 8) (NBase (NBParen (Paren (Expr (BinOp (promote (Value 20)) [(Add, promote (Die (Value 10)))]))))))
  ]

-- | Small help page for gen char.
gencharHelp :: HelpPage
gencharHelp =
  HelpPage
    "genchar"
    []
    "generate stat arrays for some systems"
    ("**Genchar**\nCan be used to generate stat arrays for certain systems.\n\nCurrently supported systems: " <> intercalate ", " (fst <$> rpgSystems) <> ".\n\n*Usage:* `genchar`, `genchar dnd`")
    []
    None

-- | The command to get the statistics for an expression and display the
-- results.
statsCommand :: Command
statsCommand = Command "stats" statsCommandParser []
  where
    oneSecond = 1000000
    statsCommandParser :: Parser (Message -> DatabaseDiscord ())
    statsCommandParser = do
      firstE <- pars
      restEs <- many (skipSpace *> pars) <* eof
      return $ statsCommand' (firstE : restEs)
    statsCommand' :: [Expr] -> Message -> DatabaseDiscord ()
    statsCommand' es m = do
      mrange' <- liftIO $ timeout (oneSecond * 5) $ mapM (\e -> rangeExpr e >>= \re -> re `seq` return (re, parseShow e)) es
      case mrange' of
        Nothing -> throwBot (EvaluationException "Timed out calculating statistics" [])
        (Just range') -> do
          mimage <- liftIO $ timeout (oneSecond * 5) (distributionByteString range' >>= \res -> res `seq` return res)
          case mimage of
            Nothing -> do
              sendMessage m (msg range')
              throwBot (EvaluationException "Timed out displaying statistics." [])
            (Just image) -> do
              liftDiscord $
                void $
                  restCall
                    ( CreateMessageDetailed
                        (messageChannelId m)
                        ( def
                            { messageDetailedContent = msg range',
                              messageDetailedFile = Just (T.unwords (snd <$> range') <> ".png", toStrict image)
                            }
                        )
                    )
      where
        msg [(d, t)] =
          if (not . isValid) d
            then "The distribution was empty."
            else
              let (modalOrder, mean, std) = getStats d
               in ( "Here are the statistics for your dice ("
                      <> formatText Code t
                      <> ").\n  Ten most common totals: "
                      <> T.pack (show (take 10 modalOrder))
                      <> "\n  Mean: "
                      <> roundShow mean
                      <> "\n  Standard deviation: "
                      <> roundShow std
                  )
        msg dts =
          let (modalOrders, means, stds) = unzip3 $ getStats . fst <$> dts
           in ( "Here are the statistics for your dice ("
                  <> intercalate ", " (formatText Code . snd <$> dts)
                  <> ").\n  Most common totals (capped to ten total): "
                  <> T.pack (show (take (div 10 (length modalOrders)) <$> modalOrders))
                  <> "\n  Means: "
                  <> intercalate ", " (roundShow <$> means)
                  <> "\n  Standard deviations: "
                  <> intercalate ", " (roundShow <$> stds)
              )
        roundShow :: Double -> Text
        roundShow d = T.pack $ show $ fromInteger (round (d * 10 ** precision)) / 10 ** precision
          where
            precision = 5 :: Double

-- | Help page for dice stats.
statsHelp :: HelpPage
statsHelp =
  HelpPage
    "stats"
    []
    "calculate and display statistics for expressions."
    "**Roll Stats**\nCan be used to display statistics for expressions of dice.\nDoes not work with \"programs\" ie multiple statements one after the other, or with accessing variables.\n\n*Usage:* `roll stats 2d20kh1`, `roll stats 4d6rr=1dl1+5`, `roll stats 3d6dl1+6 4d6dl1`"
    []
    None

-- | @rollPlugin@ assembles the command into a plugin.
rollPlugin :: Plugin
rollPlugin =
  (plug "roll")
    { commands = [rollDice, commandAlias "r" rollDice, genchar],
      helpPages = [rollHelp, gencharHelp],
      inlineCommands = [rollDiceInline],
      onComponentRecvs = [rerollComponentRecv],
      applicationCommands = catMaybes [makeApplicationCommandPair "roll" "roll some dice with a description" rollSlashCommandFunction]
    }
