{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day7Lib where

import Text.Parsec (many1)
import Text.Parsec.Combinator (sepBy, between)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Char
import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Data.List (intersect)

data Sequence = HyperNet String
              | Supernet String deriving (Show, Eq)

parseLine :: Parser [Sequence]
parseLine = many1 $
  HyperNet <$> between (char '[') (char ']') (many1 letter) <|>
  Supernet <$> many1 letter


detectABBA :: Bool -> String -> Bool
detectABBA tf (a1 : b1 : b2 : a2 : theRest) =
  tf || detectABBA (a1 == a2 && b1 == b2 && a1 /= b1 ) (b1 : b2 : a2 : theRest)
detectABBA tf _ = tf

produceXYX :: [(Char, Char, Char)] -> String -> [(Char, Char, Char)]
produceXYX acc (x1 : y1 : x2 : theRest) | x1 == x2 && x1 /= y1 =
  acc <> produceXYX [(x1, y1, x2)] (y1 : x2 : theRest)
produceXYX acc (x1 : y1 : x2 : theRest) =
  acc <> produceXYX [] (y1 : x2 : theRest)
produceXYX acc _ = acc


screenABBA =
  (foldl detectABBA False *** foldl detectABBA False) . partition

screenXYX =
  (foldl produceXYX [] *** foldl produceXYX []) . partition

xyx2yxy (x, y, _) = (y, x, y)

partition :: [Sequence] -> ([String], [String])
partition sq =
  (getData <$> filter (\case
    HyperNet s -> True
    Supernet _ -> False) sq
  ,getData <$> filter (\case
    HyperNet s -> False
    Supernet _ -> True) sq)

getData :: Sequence -> String
getData = \case
  HyperNet s -> s
  Supernet s -> s

solution = do
  input <- lines <$> readFile "src/day7/input"
  let parsed = parse parseLine "" `traverse` input
  pure $ do
    sequences <- parsed
    let part1 = screenABBA <$> sequences
    let count1 = length $ filter (\(x, y) -> not x && y) part1
    let part2 = screenXYX <$> sequences
    let count2 = length $ filter passesSSL part2
    pure count2

passesSSL :: ([(Char, Char, Char)], [(Char, Char, Char)]) -> Bool
passesSSL (xyxs,  yxys) = let
  hyperNets = xyx2yxy <$> xyxs
  superNets = yxys
  in not $ null $ hyperNets `intersect` superNets
