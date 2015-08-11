--------------------------------------------------------------------
-- |
-- Module    : Text.Regex.Applicative.Text
-- Copyright : (c) 2015 Oleg Grenrus
-- License   : BSD3
--
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability : experimental
--
-- @Text.Regex.Applicative@ API specialised to 'Char' and 'Text'.
--------------------------------------------------------------------
{-# LANGUAGE Safe #-}
module Text.Regex.Applicative.Text 
  (
  -- * Types
    RE'
  , R.RE
  -- * Smart constructors
  , sym
  , psym
  , msym
  , anySym
  , string
  , reFoldl
  , R.Greediness(..)
  , few
  , withMatched
  -- * Basic matchers
  , match
  , (=~)
  , replace
  -- * Advanced matchers
  , findFirstPrefix
  , findLongestPrefix
  , findShortestPrefix
  , findFirstInfix
  , findLongestInfix
  , findShortestInfix
  -- * Module re-exports
  , module Control.Applicative
  ) where

import           Control.Applicative
import           Control.Arrow
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Regex.Applicative as R

-- | Convenience alias for 'RE' working (also) on 'Text'.
type RE' a = R.RE Char a

-- | Match and return a single 'Char' which satisfies the predicate
psym :: (Char -> Bool) -> RE' Char
psym = R.psym

-- | Like 'psym', but allows to return a computed value instead of the
-- original symbol
msym :: (Char -> Maybe a) -> RE' a
msym = R.msym

-- | Match and return the given symbol
sym :: Char -> RE' Char
sym = R.sym

-- | Match and return any single symbol
anySym :: RE' Char
anySym = R.anySym

-- | Match and return the given 'Text'.
--
--
-- > import Text.Regex.Applicative
-- >
-- > number = string "one" *> pure 1 <|> string "two" *> pure 2
-- >
-- > main = print $ "two" =~ number
string :: Text -> RE' Text
string = fmap T.pack . R.string . T.unpack

-- | Match zero or more instances of the given expression, which are combined using
-- the given folding function.
--
-- 'Greediness' argument controls whether this regular expression should match
-- as many as possible ('Greedy') or as few as possible ('NonGreedy') instances
-- of the underlying expression.
reFoldl :: R.Greediness -> (b -> a -> b) -> b -> RE' a -> RE' b
reFoldl = R.reFoldl

-- | Match zero or more instances of the given expression, but as
-- few of them as possible (i.e. /non-greedily/). A greedy equivalent of 'few'
-- is 'many'.x
--
-- > >>> findFirstPrefix (few anySym  <* "b") "ababab"
-- > Just ("a","abab")
-- > >>> findFirstPrefix (many anySym  <* "b") "ababab"
-- > Just ("ababa","")
few :: RE' a -> RE' [a]
few = R.few

-- | Return matched symbols as part of the return value
withMatched :: RE' a -> RE' (a, Text)
withMatched = fmap (second T.pack) . R.withMatched

-- | @s =~ a = match a s@
(=~) :: Text -> RE' a -> Maybe a
(=~) = flip match
infix 2 =~

-- | Attempt to match a 'Text' against the regular expression.
-- Note that the whole string (not just some part of it) should be matched.
--
-- > >>> match (sym 'a' <|> sym 'b') "a"
-- > Just 'a'
-- > >>> match (sym 'a' <|> sym 'b') "ab"
-- > Nothing
--
match :: RE' a -> Text -> Maybe a
match = reTextF R.match

-- | Find a string prefix which is matched by the regular expression.
--
-- Of all matching prefixes, pick one using left bias (prefer the left part of
-- '<|>' to the right part) and greediness.
--
-- This is the match which a backtracking engine (such as Perl's one) would find
-- first.
--
-- If match is found, the rest of the input is also returned.
--
-- > >>> findFirstPrefix ("a" <|> "ab") "abc"
-- > Just ("a","bc")
-- > >>> findFirstPrefix ("ab" <|> "a") "abc"
-- > Just ("ab","c")
-- > >>> findFirstPrefix "bc" "abc"
-- > Nothing
findFirstPrefix :: RE' a -> Text -> Maybe (a, Text)
findFirstPrefix = fmap pairF .: reTextF R.findFirstPrefix

-- | Find the longest string prefix which is matched by the regular expression.
--
-- Submatches are still determined using left bias and greediness, so this is
-- different from POSIX semantics.
--
-- If match is found, the rest of the input is also returned.
--
--
-- > >>> let keyword = "if"
-- > >>> let identifier = many $ psym isAlpha
-- > >>> let lexeme = (Left <$> keyword) <|> (Right <$> identifier)
-- > >>> findLongestPrefix lexeme "if foo"
-- > Just (Left "if"," foo")
-- > >>> findLongestPrefix lexeme "iffoo"
-- > Just (Right "iffoo","")
findLongestPrefix :: RE' a -> Text -> Maybe (a, Text)
findLongestPrefix = fmap pairF .: reTextF R.findLongestPrefix

-- | Find the shortest prefix (analogous to 'findLongestPrefix')
findShortestPrefix :: RE' a -> Text -> Maybe (a, Text)
findShortestPrefix = fmap pairF .: reTextF R.findShortestPrefix

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findFirstPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findFirstInfix :: RE' a -> Text -> Maybe (Text, a, Text)
findFirstInfix = fmap tripleF .: reTextF R.findFirstInfix

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findLongestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findLongestInfix :: RE' a -> Text -> Maybe (Text, a, Text)
findLongestInfix = fmap tripleF .: reTextF R.findLongestInfix

-- | Find the leftmost substring that is matched by the regular expression.
-- Otherwise behaves like 'findShortestPrefix'. Returns the result together with
-- the prefix and suffix of the string surrounding the match.
findShortestInfix :: RE' a -> Text -> Maybe (Text, a, Text)
findShortestInfix = fmap tripleF .: reTextF R.findShortestInfix

-- | Replace matches of regular expression with it's value.
--
-- > >>> replace ("!" <$ sym 'f' <* some (sym 'o')) "quuxfoofooooofoobarfobar"
-- > "quux!!!bar!bar"
replace :: RE' Text -> Text -> Text
replace r = go . T.unpack
  where go :: String -> Text
        go [] = T.empty
        go ys@(x:xs) = case R.findLongestPrefix r ys of
                         Nothing              -> T.cons x (go xs)
                         Just (prefix, rest)  -> prefix <> go rest

-- Helpers

reTextF :: (a -> String -> b) -> (a -> Text -> b)
reTextF f a s = f a (T.unpack s)
{- INLINE reTextF -}

pairF :: (a, String) -> (a, Text)
pairF (x, y) = (x, T.pack y)
{-# INLINE pairF #-}

tripleF :: (String, a, String) -> (Text, a, Text)
tripleF (x, y, z) = (T.pack x, y, T.pack z)
{-# INLINE tripleF #-}

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \a b -> f (g a b)
{-# INLINE (.:) #-}
