{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Exception    (bracket)
import           Control.Lens         (view, (&), (+~), (-~), (.~), (<+~),
                                       (<-~), (^.))
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified System.Console.ANSI  as ANSI
import           System.Exit          (exitSuccess)
import           System.IO
import           Text.Emoji

import qualified Emoji.Cli.Actions    as Actions
import qualified Emoji.Cli.Emoji      as Emoji
import           Emoji.Cli.Events
import           Emoji.Cli.Options    (oExitOnCopy, parseOptions)
import           Emoji.Cli.Suggestion
import           Emoji.Cli.Types
import qualified Emoji.Cli.UI         as UI

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
          Nothing

app :: AppConfig -> IO ()
app config = loop (initialState config)
    where
    loop :: AppState -> IO ()
    loop state = do
      ANSI.setCursorPosition (config^.startingPosition.cursorPositionRow)
                             (config^.startingPosition.cursorPositionColumn)
      ANSI.clearFromCursorToScreenEnd

      UI.showPromptLine state
      UI.showEmojis config state
      state^.printStatusBar

      newState <- getEvent >>= \case
          EventQuit      -> exitSuccess
          EventEnter     -> Actions.copyEmojiToClipboard config state
          EventInput c   -> Actions.appendCharacter c config state
          EventMoveRight -> Actions.increaseScrollOffset config state
          EventMoveLeft  -> Actions.decreaseScrollOffset config state
          EventBackspace -> Actions.deleteCharacter config state
          EventCompleteSuggestion -> Actions.completeSuggestion config state


      let newEmojiList = Emoji.getEmojisForQuery (newState^.query)
      loop $ newState & emojiList .~ newEmojiList
                      & currentSuggestion .~ getSuggestionForPrompt (newState^.query)
                      & scrollOptions.scrollOffset
                      .~ (if newEmojiList /= state^.emojiList
                            then 0
                            else newState^.scrollOptions.scrollOffset)

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
        ANSI.setCursorPosition (config^.startingPosition.cursorPositionRow)
                               (config^.startingPosition.cursorPositionColumn)
        ANSI.clearFromCursorToScreenEnd
        hSetBuffering stdin $ config^.stdinBuffering
        hSetBuffering stdout $ config^.stdoutBuffering
        hSetEcho stdin True
        ANSI.showCursor
