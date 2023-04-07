{-# LANGUAGE TupleSections #-}
module Emoji.Cli.Emoji
(
      getEmojisForQuery
    , allEmojiAliases
)
where

import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Text.Emoji

getEmojisForQuery :: Text -> [Text]
getEmojisForQuery ""    = []
getEmojisForQuery query = map fst
                        . filter (\(_, aliases) -> any (T.isInfixOf query) aliases)
                        . mapMaybe (\e -> fmap (e,) (aliasesFromEmoji e))
                        $ baseEmojis

allEmojiAliases :: [Text]
allEmojiAliases = concat . mapMaybe aliasesFromEmoji $ baseEmojis
