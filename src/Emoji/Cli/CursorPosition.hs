{-# LANGUAGE TemplateHaskell #-}
module Emoji.Cli.CursorPosition
(
      CursorPosition
    , mkCursorPosition
    , cursorPositionRow
    , cursorPositionColumn
)
where

import           Control.Lens (makeLenses)

data CursorPosition = CursorPosition
  { _cursorPositionRow    :: !Int
  , _cursorPositionColumn :: !Int
  }

mkCursorPosition :: Int -> Int -> CursorPosition
mkCursorPosition = CursorPosition

makeLenses ''CursorPosition
