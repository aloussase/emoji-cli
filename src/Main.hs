{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import           Control.Exception    (bracket)
import           Control.Lens         ((&), (.~), (^.))
import qualified System.Console.ANSI  as ANSI
import           System.Exit          (exitSuccess)

import qualified Emoji.Cli.Actions    as Actions
import qualified Emoji.Cli.Config     as Config
import qualified Emoji.Cli.Emoji      as Emoji
import           Emoji.Cli.Events
import           Emoji.Cli.Suggestion
import           Emoji.Cli.Types
import qualified Emoji.Cli.UI         as UI

app :: AppConfig -> IO ()
app config = loop $ Config.initialState config
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
      let newScrollOffset = if newEmojiList /= state^.emojiList
                                then 0
                                else newState^.scrollOptions.scrollOffset

      loop $ newState & emojiList .~ newEmojiList
                      & currentSuggestion .~ getSuggestionForPrompt (newState^.query)
                      & scrollOptions.scrollOffset .~  newScrollOffset

main :: IO ()
main = bracket Config.setup Config.teardown app
