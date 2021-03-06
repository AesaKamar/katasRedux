module Day3Lib where

import Text.Parsec (spaces)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (parse)
import Text.Parsec.Char (char)
import Control.Monad.Trans.Either
import Data.List

isValidTriple :: (Num a, Ord a) => (a, a, a) -> Bool
isValidTriple (x, y, z) =
  (x + y) > z &&
  (x + z) > y &&
  (y + z) > x

parseTriple :: Parser (Int, Int, Int)
parseTriple = do
  x <- int
  _ <- spaces
  y <- int
  _ <- spaces
  z <- int
  pure (x, y, z)

matrixIdentity :: (Num a, Ord a) => [(a, a, a)] -> [(a, a, a)]
matrixIdentity = id

matrixTranspose :: (Num a, Ord a) => [(a, a, a)] -> [(a, a, a)]
matrixTranspose [] = []
matrixTranspose ((a1, a2, a3) :
                 (a4, a5, a6) :
                 (a7, a8, a9) : rest ) =
                              (a1, a4, a7) :
                              (a2, a5, a8) :
                              (a3, a6, a9) : matrixTranspose rest


solution :: ([(Int, Int, Int)] -> [(Int, Int, Int)]) -> IO ()
solution rotation = do
  instring <- lines <$> readFile "src/day3/input"
  answer <- pure $ do
    triples <- traverse (parse parseTriple "") instring
    pure $ length . filter isValidTriple $ (rotation triples)
  print answer


part1 = solution matrixIdentity
part2 = solution matrixTranspose
