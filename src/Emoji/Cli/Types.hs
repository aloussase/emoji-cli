{-# LANGUAGE TemplateHaskell #-}
module Emoji.Cli.Types where

import           Control.Lens
import           Data.Text    (Text)
import           System.IO


data AppConfig = AppConfig
  { _appConfigStartingPosition :: !CursorPosition
  , _appConfigStdoutBuffering  :: !BufferMode
  , _appConfigStdinBuffering   :: !BufferMode
  , _appConfigTerminalSize     :: !TerminalSize
  , _appConfigExitOnCopy       :: !Bool
  }

data AppState = AppState
  { _appStateQuery          :: !Text
  , _appStateCursorPosition :: !CursorPosition
  , _appStateScrollOptions  :: !ScrollOptions
  , _appStateEmojiList      :: ![Text]
  , _appStatePrintStatusBar :: !(IO ())
  }

data TerminalSize = TerminalSize
  { _terminalSizeRows    :: !Int
  , _terminalSizeColumns :: !Int
  }

data CursorPosition = CursorPosition
  { _cursorPositionRow    :: !Int
  , _cursorPositionColumn :: !Int
  }


data ScrollOptions = ScrollOptions
  { _scrollOffset     :: !Int
  , _scrollWindowSize :: !Int
  }

makeLenses ''ScrollOptions
makeLenses ''TerminalSize
makeLenses ''CursorPosition
makeFields ''AppState
makeFields ''AppConfig
