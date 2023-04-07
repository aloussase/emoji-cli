module Emoji.Cli.Options
(
      Options (..)
    , parseOptions
)
where

import qualified Options.Applicative as OA

newtype Options = Options
    { oExitOnCopy :: Bool
    }
    deriving Show


options :: OA.Parser Options
options = Options
    <$> (OA.switch $
            OA.long "exit-on-copy" <>
            OA.short 'e' <>
            OA.help "Exit after copying an emoji to the clipboard")

parserInfo :: OA.ParserInfo Options
parserInfo = OA.info (OA.helper <*> options) $
    OA.fullDesc <>
    OA.header "emoji-cli - Emoji cli tool for the command line" <>
    OA.progDescDoc (Just desc)
    where
        desc = mconcat
            [ "Search for emoji and copy to the clipboard\n\n"
            , "Controls:\n"
            , "- Scroll to the left:        Left arrow\n"
            , "- Scroll to the right:       Right arrow\n"
            , "- Autocomplete:              Tab"
            , "- Copy the selected emoji:   Enter\n"
            , "- Quit:                      q\n"
            ]

parseOptions :: IO Options
parseOptions = OA.execParser parserInfo

