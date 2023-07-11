-- -- |
-- Module      : Tablebot.Utility.Help
-- Description : Help text generation and storage
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module creates functions and data structures to help generate help text for commands
module Tablebot.Utility.Help where

import Data.Default (Default (def))
import Data.Text (Text)
import qualified Data.Text as T
import Tablebot.Internal.Permission (getSenderPermission, userHasPermission)
import Tablebot.Internal.Plugins (changeAction)
import Tablebot.Internal.Types
import Tablebot.Utility.Discord (Message, sendMessage)
import Tablebot.Utility.Parser (skipSpace1)
import Tablebot.Utility.Permission (requirePermission)
import Tablebot.Utility.Types hiding (helpPages)
import Text.Megaparsec (choice, chunk, eof, (<|>))

helpHelpPage :: HelpPage
helpHelpPage = HelpPage "help" [] "show information about commands" "**Help**\nShows information about bot commands\n\n*Usage:* `help <page>`" [] None

generateHelp :: Text -> CombinedPlugin -> CombinedPlugin
generateHelp rootText p =
  p
    { combinedSetupAction = return (def {compiledCommands = [CCommand "help" (handleHelp rootText (helpHelpPage : combinedHelpPages p)) []]}) : combinedSetupAction p
    }

handleHelp :: Text -> [HelpPage] -> Parser (Message -> CompiledDatabaseDiscord ())
handleHelp rootText hp = base <|> parsePages
  where
    root = HelpPage "" [] "" rootText hp None
    base = eof >> pure (displayHelp root) -- base help command
    parsePages = choice $ map parseHelpPage hp -- one of the commands we're asking for help on

parseHelpPage :: HelpPage -> Parser (Message -> CompiledDatabaseDiscord ())
parseHelpPage hp = do
  _ <- choice (map chunk (helpName hp : helpAliases hp)) -- we parse out the command name
  base <|> subcommands
  where
    base = eof >> pure (displayHelp hp)
    subcommands = skipSpace1 >> choice (map parseHelpPage $ helpSubpages hp)

displayHelp :: HelpPage -> Message -> CompiledDatabaseDiscord ()
displayHelp hp m = changeAction () . requirePermission (helpPermission hp) m $ do
  uPerm <- getSenderPermission m
  sendMessage m $ formatHelp uPerm hp

formatHelp :: UserPermission -> HelpPage -> Text
formatHelp up hp = helpBody hp <> formatSubpages hp
  where
    formatSubpages :: HelpPage -> Text
    formatSubpages (HelpPage _ _ _ _ [] _) = ""
    formatSubpages hp' = if T.null sp then "" else "\n\n*Subcommands*" <> sp
      where
        sp = T.concat (map formatSubpage (helpSubpages hp'))
    formatSubpage :: HelpPage -> Text
    formatSubpage hp' = if userHasPermission (helpPermission hp') up then "\n`" <> helpName hp' <> "` " <> helpShortText hp' else ""
