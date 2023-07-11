-- |
-- Module      : Tablebot.Internal.Handler.Command
-- Description : The event handler for received messages.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module deals with 'Command's and 'InlineCommand's, checking for the
-- command prefix in the case of 'Command's and then trying each plugin-defined
-- parser to see if it matches.
module Tablebot.Internal.Handler.Command
  ( parseNewMessage,
    parseCommands,
    parseInlineCommands,
    parseValue,
  )
where

import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Set (singleton, toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Discord.Types (Message (messageAuthor, messageContent), User (userId))
import Tablebot.Internal.Alias
import Tablebot.Internal.Plugins (changeAction)
import Tablebot.Internal.Types
import Tablebot.Utility.Discord (sendEmbedMessage)
import Tablebot.Utility.Exception (BotException (ParserException), embedError, throwBot)
import Tablebot.Utility.Parser (skipSpace1, space, word)
import Tablebot.Utility.Types (EnvDatabaseDiscord, Parser)
import Text.Megaparsec
import qualified UnliftIO.Exception as UIOE (tryAny)

-- | @parseNewMessage@ parses a new message, first by attempting to match the
-- bot's prefix to the start of the message, then (if that fails) by attempting
-- to find inline commands.
parseNewMessage :: PluginActions -> Text -> Message -> CompiledDatabaseDiscord ()
parseNewMessage pl prefix m =
  if isCommandCall $ messageContent m
    then parseCommands (compiledCommands pl) m prefix
    else parseInlineCommands (compiledInlineCommands pl) m
  where
    -- We assume that if someone types .singleword, that is a command.
    -- Otherwise we ignore it (e.g. "... so what" is not a command).
    isCommandCall :: Text -> Bool
    isCommandCall t = case parse checkCommand "" t of
      Left _ -> False
      Right _ -> True
    checkCommand :: Parser ()
    checkCommand = chunk prefix *> word *> (space <|> eof)

parseCommands :: [CompiledCommand] -> Message -> Text -> CompiledDatabaseDiscord ()
parseCommands cs m prefix = do
  as <- changeAction () $ getAliases (userId $ messageAuthor m)
  res <- parseCommands' cs as m prefix
  case res of
    Right _ -> return ()
    Left (title, e) -> changeAction () . sendEmbedMessage m "" $ embedError $ ParserException (T.unpack title) . T.unpack $ "```\n" <> e <> "```"

-- | Given a list of 'Command' @cs@, the 'Message' that triggered the event
-- @m@, and a command prefix @prefix@, construct a parser that parses commands.
-- We look for the prefix, followed by trying out the name of each command,
-- then on finding a valid command name running that command's parser to get a
-- runnable function @Message -> DatabaseDiscord ()@ which is then run.
--
-- If the parser errors, the last error (which is hopefully one created by
-- '<?>') is sent to the user as a Discord message.
parseCommands' :: [CompiledCommand] -> Maybe [Alias] -> Message -> Text -> CompiledDatabaseDiscord (Either (Text, Text) ())
parseCommands' cs as m prefix = case parse (parser cs) "" (messageContent m) of
  Right p -> Right <$> p m
  Left e -> case as of
    (Just as'@(_ : _)) ->
      case parse (aliasParser as') "" (messageContent m) of
        -- if the alias parser fails, just give the outer error
        Left _ -> mkTitleBody e
        -- if we get a valid alias, run the command with the alias
        -- the way we do this is by running this function again and edit the
        -- message text to be the alias's command
        -- we ensure no infinite loops by removing the alias we just used
        Right (a', rest) -> do
          recur <- parseCommands' cs (Just $ filter (/= a') as') (m {messageContent = prefix <> aliasCommand a' <> rest}) prefix
          -- if successful, return the result. if not, edit the error we
          -- obtained from running the alias to include the alias we tried to
          -- use
          case recur of
            Right _ -> return recur
            Left (title, err) -> return $ Left (title, aliasAlias a' <> " -> " <> aliasCommand a' <> "\n" <> err)
    _ -> mkTitleBody e
  where
    mkTitleBody e' = let (errs, title) = makeBundleReadable e' in return $ Left (T.pack title, T.pack $ errorBundlePretty errs)
    parser :: [CompiledCommand] -> Parser (Message -> CompiledDatabaseDiscord ())
    parser cs' =
      do
        _ <- chunk prefix
        choice (map commandToParser cs') <?> "No command with that name was found!"
    commandToParser :: CompiledCommand -> Parser (Message -> CompiledDatabaseDiscord ())
    commandToParser c = try (chunk (commandName c) *> (skipSpace1 <|> eof)) *> (try (choice $ map commandToParser $ commandSubcommands c) <|> commandParser c)
    aliasParser :: [Alias] -> Parser (Alias, Text)
    aliasParser as' = do
      _ <- chunk prefix
      a <- choice (map (chunk . aliasAlias) as') <?> "No command with that name was found!"
      rst <- many anySingle
      case find (\a' -> aliasAlias a' == a) as' of
        Just a' -> return (a', T.pack rst)
        Nothing -> fail "This should never happen! (aliasParser)"

data ReadableError = UnknownError | KnownError String [String]
  deriving (Show, Eq, Ord)

instance ShowErrorComponent ReadableError where
  showErrorComponent UnknownError = "Unknown error!"
  showErrorComponent (KnownError l possibles) =
    l <> ending
    where
      ending = if null possibles then "" else "\nMaybe you meant one of: " <> commaSep possibles
      commaSep :: [String] -> String
      commaSep [] = ""
      commaSep [x] = x <> "."
      commaSep [x, y] = x <> " or " <> y <> "."
      commaSep (x : xs) = x <> ", " <> commaSep xs

makeBundleReadable :: ParseErrorBundle Text Void -> (ParseErrorBundle Text ReadableError, String)
makeBundleReadable (ParseErrorBundle errs state) =
  let (errors, title) = NE.unzip $ NE.map makeReadable errs
   in (ParseErrorBundle errors state, getTitle $ NE.toList title)
  where
    getTitle :: [Maybe String] -> String
    -- Safety proof for application of `head`: we filter by `not . null` so each element is nonempty.
    getTitle titles = case filter (not . null) $ catMaybes titles of
      -- therefore, `x` is nonempty, so `lines x` is nonempty, meaning that `head (lines x)` is fine,
      -- since `lines x` is nonempty for nonempty input.
      (x : xs) ->
        let title = head (lines x)
         in if null xs then title else title ++ " (and " ++ show (length xs) ++ " more)"
      [] -> "Parser Error!"

-- | Transform our errors into more useful ones.
-- This uses the Label hidden within each error to build an error message,
-- as we have used labels to give parsers user-facing errors.
makeReadable :: ParseError Text Void -> (ParseError Text ReadableError, Maybe String)
makeReadable (TrivialError i _ good) =
  let (lab, others) = getLabel (toList good)
   in case lab of
        Just l -> (FancyError i . singleton . ErrorCustom $ KnownError l others, Just l)
        Nothing -> (FancyError i . singleton $ ErrorCustom $ KnownError "Unknown error without label" others, Nothing)
  where
    getLabel :: [ErrorItem (Token Text)] -> (Maybe String, [String])
    getLabel [] = (Nothing, [])
    getLabel ((Tokens nel) : xs) = (Nothing, [NE.toList nel]) <> getLabel xs
    getLabel ((Label ls) : xs) = (Just (NE.toList ls <> "\n"), []) <> getLabel xs
    getLabel (EndOfInput : xs) = (Nothing, ["no more input"]) <> getLabel xs
makeReadable e = (mapParseError (const UnknownError) e, Nothing)

-- | Given a list of 'InlineCommand' @cs@ and a message @m@, run each inline
-- command's parser on the message text. Errors are not sent to the user, and do
-- not halt command attempts (achieved using 'tryAny').
parseInlineCommands :: [CompiledInlineCommand] -> Message -> CompiledDatabaseDiscord ()
parseInlineCommands cs m = mapM_ (fromResult . (\cic -> parse (inlineCommandParser cic) "" (messageContent m))) cs
  where
    fromResult (Right p) = UIOE.tryAny (p m)
    fromResult _ = return $ return ()

-- | Turn the parsing of a value into an exception when given text to parse.
parseValue :: Parser a -> Text -> EnvDatabaseDiscord s a
parseValue par t = case parse par "" t of
  Right p -> return p
  Left e ->
    let (errs, title) = makeBundleReadable e
     in throwBot $ ParserException title $ "```\n" ++ errorBundlePretty errs ++ "```"
