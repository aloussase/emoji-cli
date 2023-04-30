{-# LANGUAGE TemplateHaskell #-}
module Emoji.Cli.Config
(
      setup
    , teardown
    -- * AppConfig
    , AppConfig
    -- * Lenses
    , startingPosition
    , stdoutBuffering
    , stdinBuffering
    , terminalSize
    , options
    -- * Fields
    , HasStartingPosition
    , HasStdoutBuffering
    , HasStdinBuffering
    , HasTerminalSize
    , HasOptions
    -- * TerminalSize
    , TerminalSize
    , terminalSizeRows
    , terminalSizeColumns
)
where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Maybe               (fromJust)
import qualified System.Console.ANSI      as ANSI
import           System.IO

import           Emoji.Cli.CursorPosition
import           Emoji.Cli.Options

data AppConfig = AppConfig
  { _appConfigStartingPosition :: !CursorPosition
  , _appConfigStdoutBuffering  :: !BufferMode
  , _appConfigStdinBuffering   :: !BufferMode
  , _appConfigTerminalSize     :: !TerminalSize
  , _appConfigOptions          :: !Options
  }

data TerminalSize = TerminalSize
  { _terminalSizeRows    :: !Int
  , _terminalSizeColumns :: !Int
  }

makeLenses ''TerminalSize
makeFields ''AppConfig

setupBuffering :: (MonadIO m) => Handle -> m BufferMode
setupBuffering handle = liftIO $ hGetBuffering handle <* hSetBuffering handle NoBuffering

setupCursor :: (MonadIO m) => m CursorPosition
setupCursor = do
    liftIO $ ANSI.saveCursor >> ANSI.hideCursor >> hSetEcho stdin False
    (row, col) <- fromJust <$> liftIO ANSI.getCursorPosition
    return $ mkCursorPosition row col

setupTerminalSize :: (MonadIO m) => m TerminalSize
setupTerminalSize = do
    (rows, cols) <- fromJust <$> liftIO ANSI.getTerminalSize
    return $ TerminalSize rows cols

setup :: (MonadIO m) => m AppConfig
setup = AppConfig
        <$> setupCursor
        <*> setupBuffering stdout
        <*> setupBuffering stdin
        <*> setupTerminalSize
        <*> liftIO parseOptions

teardown :: (MonadIO m) => AppConfig -> m ()
teardown config = liftIO $ do
    ANSI.restoreCursor
    ANSI.setCursorPosition (config^.startingPosition.cursorPositionRow) (config^.startingPosition.cursorPositionColumn)
    ANSI.clearFromCursorToScreenEnd
    hSetBuffering stdin  $ config^.stdinBuffering
    hSetBuffering stdout $ config^.stdoutBuffering
    hSetEcho stdin True
    ANSI.showCursor
