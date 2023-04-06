module Emoji.Cli.Events where

import           System.IO

data Event = EventQuit
           | EventInput Char
           | EventBackspace
           | EventMoveRight
           | EventMoveLeft
           | EventEnter
           deriving Show

getEvent :: IO Event
getEvent =
  getChar >>= \case
    '\ESC' -> do
      getChar >> getChar >>= \case
        'C' -> pure EventMoveRight
        'D' -> pure EventMoveLeft
        c   -> pure $ EventInput c
    'q'    -> pure EventQuit
    '\n'   -> pure EventEnter
    '\DEL' -> pure EventBackspace
    c      -> pure $ EventInput c
