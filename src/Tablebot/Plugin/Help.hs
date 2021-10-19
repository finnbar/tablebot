-- -- |
-- Module      : Tablebot.Plugin.Help
-- Description : Help text generation and storage
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module creates functions and data structures to help generate help text for commands
module Tablebot.Plugin.Help where

import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Tablebot.Handler.Permission (getSenderPermission, userHasPermission)
import Tablebot.Plugin.Discord (Message, sendMessage)
import Tablebot.Plugin.Parser (skipSpace)
import Tablebot.Plugin.Permission (requirePermission)
import Tablebot.Plugin.Types
import Text.Megaparsec (choice, chunk, eof, try, (<?>), (<|>))

rootBody :: Text
rootBody =
  "**Tabletop Bot**\n\
  \This friendly little bot provides several tools to help with\
  \ the running of the Warwick Tabletop Games and Role-Playing Society Discord server."

helpHelpPage :: HelpPage
helpHelpPage = HelpPage "help" "show information about commands" "**Help**\nShows information about bot commands\n\n*Usage:* `help <page>`" [] None

generateHelp :: Plugin -> Plugin
generateHelp p =
  p
    { commands = Command "help" (handleHelp (helpHelpPage : helpPages p)) : commands p
    }

handleHelp :: [HelpPage] -> Parser (Message -> DatabaseDiscord ())
handleHelp hp = parseHelpPage root
  where
    root = HelpPage "" "" rootBody hp None

parseHelpPage :: HelpPage -> Parser (Message -> DatabaseDiscord ())
parseHelpPage hp = do
  _ <- chunk (helpName hp)
  skipSpace
  (try eof $> displayHelp hp) <|> choice (map parseHelpPage $ helpSubpages hp) <?> "Unknown Subcommand"

displayHelp :: HelpPage -> Message -> DatabaseDiscord ()
displayHelp hp m = requirePermission (helpPermission hp) m $ do
  uPerm <- getSenderPermission m
  sendMessage m $ formatHelp uPerm hp

formatHelp :: UserPermission -> HelpPage -> Text
formatHelp up hp = helpBody hp <> formatSubpages hp
  where
    formatSubpages :: HelpPage -> Text
    formatSubpages (HelpPage _ _ _ [] _) = ""
    formatSubpages hp' = if T.null sp then "" else "\n\n*Subcommands*" <> sp
      where
        sp = T.concat (map formatSubpage (helpSubpages hp'))
    formatSubpage :: HelpPage -> Text
    formatSubpage hp' = if userHasPermission (helpPermission hp') up then "\n`" <> helpName hp' <> "` " <> helpShortText hp' else ""
