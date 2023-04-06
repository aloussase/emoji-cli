{-# LANGUAGE TupleSections #-}
module Emoji.Cli.Emoji where

import           Data.Maybe (mapMaybe)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Text.Emoji

getEmojisForQuery :: Text -> [Text]
getEmojisForQuery ""    = []
getEmojisForQuery query = map fst
                        . filter (\(_, aliases) -> any (T.isInfixOf query) aliases)
                        . mapMaybe (\e -> fmap (e,) (aliasesFromEmoji e))
                        $ baseEmojis
