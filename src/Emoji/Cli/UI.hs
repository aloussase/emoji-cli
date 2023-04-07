module Emoji.Cli.UI
(
    showEmojis
  , showHelpMessage
  , showPromptLine
  , atStatusBar
) where

import           Control.Lens        (_2, ix, makeLenses, over, (&), (+~),
                                      (<+~), (^.), (^?))
import           Control.Monad       (forM_, when)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified System.Console.ANSI as ANSI

import           Emoji.Cli.Types


printPlaceHolderText :: Text -> IO ()
printPlaceHolderText text = do
    ANSI.setSGR [ANSI.SetPaletteColor ANSI.Foreground (ANSI.xterm24LevelGray 12)]
    TIO.putStr text
    ANSI.setSGR [ANSI.Reset]

printColored :: ANSI.ColorIntensity -> ANSI.Color -> Text -> IO ()
printColored intensity color text = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground intensity color]
    TIO.putStr text
    ANSI.setSGR [ANSI.Reset]

atStatusBar ::
    (HasTerminalSize config TerminalSize, HasStartingPosition config CursorPosition)
    => config
    -> IO ()
    -> IO ()
atStatusBar config action = do
    ANSI.setCursorPosition (config^.terminalSize.terminalSizeRows - 1) 0
    action
    ANSI.setCursorPosition (config^.startingPosition.cursorPositionRow)
                           (config^.startingPosition.cursorPositionColumn)

showPromptLine :: (HasQuery state Text, HasCurrentSuggestion state (Maybe Text)) => state -> IO ()
showPromptLine state = do
    printColored ANSI.Vivid ANSI.Cyan "\xf054 "
    if T.null $ state^.query
      then printPlaceHolderText "Type here to start searching for emoji"
      else do let suggestion = fromMaybe "" (state^.currentSuggestion)
              TIO.putStr $ state^.query
              printPlaceHolderText suggestion

showHelpMessage :: (HasTerminalSize config TerminalSize, HasStartingPosition config CursorPosition) => config -> IO ()
showHelpMessage config = atStatusBar config $ do
    putStr "Press "
    printColored ANSI.Vivid ANSI.Green "'q'"
    putStr " to exit, "
    printColored ANSI.Vivid ANSI.Green "'enter'"
    putStr " to copy the selected emoji, use the arrows to move left and right"

showEmojis ::
  ( HasTerminalSize config TerminalSize
  , HasStartingPosition config CursorPosition
  , HasCursorPosition state CursorPosition
  , HasScrollOptions state ScrollOptions
  , HasEmojiList state [Text]
  )
  => config
  -> state
  -> IO ()
showEmojis config state = do
    let windowSize = min (state^.scrollOptions.scrollWindowSize) (config^.terminalSize.terminalSizeColumns)
    let (_, es) = over _2 (take windowSize) $ splitAt (state^.scrollOptions.scrollOffset) (state^.emojiList)
    ANSI.setCursorPosition (config^.startingPosition.cursorPositionRow + 1) 0
    go es True
    where
      go [] _ = pure ()
      go (e:es) drawingFirstEmoji
        | drawingFirstEmoji = printColored ANSI.Vivid ANSI.Yellow e >> go es False
        | otherwise = TIO.putStr e >> go es False
