{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Exception        (bracket)
import           Control.Lens             ((&), (.~), (^.))
import qualified System.Console.ANSI      as ANSI

import           Emoji.Cli.Config
import qualified Emoji.Cli.Config         as Config
import           Emoji.Cli.CursorPosition
import qualified Emoji.Cli.Emoji          as Emoji
import           Emoji.Cli.Events
import           Emoji.Cli.State
import qualified Emoji.Cli.State          as State
import           Emoji.Cli.State.Update
import           Emoji.Cli.Suggestion
import qualified Emoji.Cli.UI             as UI

app :: AppConfig -> IO ()
app config = loop $ State.fromConfig config (UI.showHelpMessage config)
    where
    loop :: AppState -> IO ()
    loop state = do
      ANSI.setCursorPosition (config^.startingPosition.cursorPositionRow)
                             (config^.startingPosition.cursorPositionColumn)
      ANSI.clearFromCursorToScreenEnd

      UI.showPromptLine state
      UI.showEmojis config state
      state^.printStatusBar

      newState <- getEvent >>= updateState config state

      let newEmojiList = Emoji.getEmojisForQuery (newState^.query)
      let newScrollOffset = if newEmojiList /= state^.emojiList
                                then 0
                                else newState^.scrollOptions.scrollOffset

      loop $ newState & emojiList .~ newEmojiList
                      & currentSuggestion .~ getSuggestionForPrompt (newState^.query)
                      & scrollOptions.scrollOffset .~  newScrollOffset

main :: IO ()
main = bracket Config.setup Config.teardown app
