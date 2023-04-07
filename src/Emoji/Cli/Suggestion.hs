module Emoji.Cli.Suggestion where

import           Data.List       (find, sortOn)
import           Data.Maybe      (listToMaybe)
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Emoji.Cli.Emoji (allEmojiAliases)

getSuggestionForPrompt :: Text -> Maybe Text
getSuggestionForPrompt prompt =
    let bestMatch = listToMaybe
                  . sortOn T.length
                  $ filter (T.isPrefixOf prompt) allEmojiAliases
     in T.drop (T.length prompt) <$> bestMatch


