{-# LANGUAGE RankNTypes #-}
module Emoji.Cli.Actions where

import           Control.Lens           (ix, (%~), (&), (.~), (?~), (^.), (^?))
import           Control.Monad.IO.Class
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           System.Exit            (exitSuccess)
import qualified System.Hclip           as Hclip

import           Emoji.Cli.Options
import           Emoji.Cli.Types
import qualified Emoji.Cli.UI           as UI

type Action = forall m. (MonadIO m) => AppConfig -> AppState -> m AppState

appendCharacter :: Char -> Action
appendCharacter c _ state = pure $ state & query %~ flip T.snoc c

deleteCharacter :: Action
deleteCharacter _ state = pure $
    if not (T.null $ state^.query)
        then state & query %~ T.init
        else state

increaseScrollOffset :: Action
increaseScrollOffset config state =
    let numberOfEmoji = length $ state^.emojiList
        maximumScrollOffset = min (state^.scrollOptions.scrollWindowSize) numberOfEmoji
        currentScrollOffset = state^.scrollOptions.scrollOffset
        newScrollOffset = min (currentScrollOffset + 1) (maximumScrollOffset - 1)
    in
        pure $ state & scrollOptions.scrollOffset .~ newScrollOffset

decreaseScrollOffset :: Action
decreaseScrollOffset config state =
    let currentScrollOffset = state^.scrollOptions.scrollOffset
        newScrollOffset = max (currentScrollOffset - 1) 0
     in
        pure $ state & scrollOptions.scrollOffset .~ newScrollOffset


copyEmojiToClipboard :: Action
copyEmojiToClipboard config state =
    let selectedEmoji = state ^. emojiList . ix (state^.scrollOptions.scrollOffset)
     in do liftIO $ Hclip.setClipboard $ T.unpack selectedEmoji
           if oExitOnCopy (config^.options)
                then liftIO exitSuccess
                else pure
                    $ state
                    & printStatusBar
                    .~ UI.atStatusBar config (TIO.putStr $ selectedEmoji <> " was copied to the clipboard")

completeSuggestion :: Action
completeSuggestion _ state = pure $ state
    & query %~ flip mappend (fromMaybe "" (state^.currentSuggestion))
    & currentSuggestion .~ Nothing
