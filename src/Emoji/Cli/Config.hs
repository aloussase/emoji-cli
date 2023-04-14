module Emoji.Cli.Config
(
      setup
    , teardown
    , initialState
)
where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Maybe             (fromJust)
import qualified System.Console.ANSI    as ANSI
import           System.IO

import           Emoji.Cli.Options
import           Emoji.Cli.Types
import qualified Emoji.Cli.UI           as UI

setupBuffering :: (MonadIO m) => Handle -> m BufferMode
setupBuffering handle = liftIO $ hGetBuffering handle <* hSetBuffering handle NoBuffering

setupCursor :: (MonadIO m) => m CursorPosition
setupCursor = do
    liftIO $ ANSI.saveCursor >> ANSI.hideCursor >> hSetEcho stdin False
    (row, col) <- fromJust <$> liftIO ANSI.getCursorPosition
    return $ CursorPosition row col

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

initialState :: AppConfig -> AppState
initialState config =
    let terminalColumns = config^.terminalSize.terminalSizeColumns
     in
        AppState
          ""
          (view startingPosition $ config & startingPosition.cursorPositionRow +~ 1)
          (ScrollOptions 0 terminalColumns)
          []
          (UI.showHelpMessage config)
          Nothing
