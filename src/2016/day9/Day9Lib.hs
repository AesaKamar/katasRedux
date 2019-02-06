{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day9Lib where

import Text.Parsec (many1, try)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator (sepBy, between)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Char
import Control.Applicative ((<|>), (*>), (<*), (<*>), liftA2, many)
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.List as L


type ExpandedFragment = String

data Fragment = Marker ((Int, Int), String) |
                NoMarker (ExpandedFragment, String)
              deriving (Show)

tup a b = (,) <$> a <*> b

markerParser :: Parser Fragment
markerParser =
  (Marker <$> between (char '(') (char ')') ((int <* char 'x') `tup` int) `tup` many anyChar)
    <|>
  try (NoMarker <$> many (noneOf "(") `tup` (many (oneOf "(") <> many anyChar) )

extract :: Fragment -> (ExpandedFragment, String)
extract (Marker ((n, rep), s)) =
  (mconcat $ replicate rep (take n s) , drop n s)
extract (NoMarker (xpanded, rest)) = (xpanded, rest)

expandAndFold :: [ExpandedFragment] ->  String -> Either ParseError [ExpandedFragment]
expandAndFold frags s = do
  fragTree <- parse markerParser "" s
  let (expandedFrag, tailToRecurse) = extract fragTree
  if null tailToRecurse
    then pure (frags <> [expandedFrag])
    else expandAndFold (frags <> [expandedFrag]) tailToRecurse

solution = do
  input <- readFile "src/day9/input"
  let expanded =  mconcat <$> expandAndFold [] input
  pure $ length <$> expanded
