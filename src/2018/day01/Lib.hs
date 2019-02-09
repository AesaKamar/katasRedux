module Day01_2018 where

import           Text.Parsec.Prim
import           Text.Parsec.Error (ParseError)
import           Text.ParserCombinators.Parsec.Number ( int )
import Data.Set (member, insert, Set, fromList)
import Common
import Data.Monoid
import Data.Semigroup

instance Semigroup Integer where
  (<>) = (+)
instance Monoid Integer where
  mempty = 0



checkAndStore :: (Ord a, Monoid a) => (a, Maybe a, Set a) -> a -> t a ->  (a, Maybe a, Set a)
checkAndStore (acc, Just a, s) n rest = (acc, Just a, s)
checkAndStore (acc, Nothing, s) n rest =
  if value `member` s
  then (value, Just value, s)
  else checkAndStore (value, Nothing, insert value s ) value rest
  where value = acc <> n

solution = do
  eachLine <-  readFile "./src/2018/day01/input" |$> lines
  let parsed = eachLine |$> parse int ""  |> sequence
  let part1 = parsed |$> foldl (+) 0
  let part2 = parsed |$> cycle1 |$> checkAndStore (mempty, mempty, mempty)
  pure part2
