{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Exception   (bracket)
import           Control.Lens        (view, (&), (+~), (-~), (.~), (<+~), (<-~),
                                      (^.))
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified System.Console.ANSI as ANSI
import           System.IO
import           Text.Emoji

import qualified Emoji.Cli.Actions   as Actions
import qualified Emoji.Cli.Emoji     as Emoji
import           Emoji.Cli.Events
import           Emoji.Cli.Options   (oExitOnCopy, parseOptions)
import           Emoji.Cli.Types
import qualified Emoji.Cli.UI        as UI

initialState :: AppConfig -> AppState
initialState appConfig =
    let terminalColumns = appConfig^.terminalSize.terminalSizeColumns
     in
        AppState
          ""
          (view startingPosition $ appConfig & startingPosition.cursorPositionRow +~ 1)
          (ScrollOptions 0 terminalColumns)
          []
          (UI.showHelpMessage appConfig)

app :: AppConfig -> IO ()
app appConfig = loop (initialState appConfig)
  where
    loop :: AppState -> IO ()
    loop currentState = do
      ANSI.setCursorPosition (appConfig^.startingPosition.cursorPositionRow)
                             (appConfig^.startingPosition.cursorPositionColumn)
      ANSI.clearFromCursorToScreenEnd

      currentState^.printStatusBar
      UI.showPromptLine currentState

      let newEmojiList = Emoji.getEmojisForQuery (currentState^.query)
          newState = currentState
                        & emojiList .~ newEmojiList
                        & scrollOptions.scrollOffset
                        .~ (if newEmojiList /= currentState^.emojiList
                                then 0
                                else currentState^.scrollOptions.scrollOffset)

      UI.showEmojis appConfig newState

      getEvent >>= \case
          EventQuit      -> pure ()
          EventEnter     -> loop =<< Actions.copyEmojiToClipboard appConfig newState
          EventInput c   -> loop =<< Actions.appendCharacter c appConfig newState
          EventMoveRight -> loop =<< Actions.increaseScrollOffset appConfig newState
          EventMoveLeft  -> loop =<< Actions.decreaseScrollOffset appConfig newState
          EventBackspace -> loop =<< Actions.deleteCharacter appConfig newState

main :: IO ()
main = bracket setup teardown app
  where
    setup = do
        options <- parseOptions
        ANSI.saveCursor
        stdinBuffering <- hGetBuffering stdin
        stdoutBuffering <- hGetBuffering stdout
        hSetBuffering stdin NoBuffering
        hSetBuffering stdout NoBuffering
        hSetEcho stdin False
        Just initialPosition@(row, col) <- ANSI.getCursorPosition
        ANSI.setCursorPosition (row + 1) col
        Just (terminalRows, terminalCols) <- ANSI.getTerminalSize
        ANSI.hideCursor
        pure $ AppConfig
            (CursorPosition row col)
            stdoutBuffering
            stdinBuffering
            (TerminalSize terminalRows terminalCols)
            (oExitOnCopy options)

    teardown config = do
        ANSI.restoreCursor
        ANSI.clearFromCursorToScreenEnd
        hSetBuffering stdin $ config^.stdinBuffering
        hSetBuffering stdout $ config^.stdoutBuffering
        hSetEcho stdin True
        ANSI.showCursor
