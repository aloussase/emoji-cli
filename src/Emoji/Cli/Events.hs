module Emoji.Cli.Events where

import           Data.Functor ((<&>))

data Event = EventQuit
           | EventInput Char
           | EventBackspace
           | EventMoveRight
           | EventMoveLeft
           | EventEnter
           | EventCompleteSuggestion
           deriving Show

getEvent :: IO Event
getEvent =
  getChar >>= \case
    '\ESC' -> do getChar >> getChar <&> \case
                    'C' -> EventMoveRight
                    'D' -> EventMoveLeft
                    c   -> EventInput c
    '\DC1' -> pure EventQuit
    '\n'   -> pure EventEnter
    '\DEL' -> pure EventBackspace
    '\t'   -> pure EventCompleteSuggestion
    c      -> pure $ EventInput c
