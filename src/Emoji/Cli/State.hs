{-# LANGUAGE TemplateHaskell #-}
module Emoji.Cli.State where

import           Control.Lens
import           Data.Text                (Text)

import           Emoji.Cli.Config
import           Emoji.Cli.CursorPosition

data ScrollOptions = ScrollOptions
  { _scrollOffset     :: !Int
  , _scrollWindowSize :: !Int
  }

data AppState = AppState
  { _appStateQuery             :: !Text
  , _appStateCursorPosition    :: !CursorPosition
  , _appStateScrollOptions     :: !ScrollOptions
  , _appStateEmojiList         :: ![Text]
  , _appStatePrintStatusBar    :: !(IO ())
  , _appStateCurrentSuggestion :: !(Maybe Text)
  }

makeLenses ''ScrollOptions
makeFields ''AppState

fromConfig :: AppConfig -> IO () -> AppState
fromConfig config statuslineM =
    let terminalColumns = config^.terminalSize.terminalSizeColumns
     in
        AppState
          ""
          (view startingPosition $ config & startingPosition.cursorPositionRow +~ 1)
          (ScrollOptions 0 terminalColumns)
          []
          statuslineM
          Nothing
