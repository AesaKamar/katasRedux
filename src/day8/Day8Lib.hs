{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}


module Day8Lib where

import Text.Parsec (many1, try)
import Text.Parsec.Combinator (sepBy, between)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Char
import Control.Applicative ((<|>), (*>), (<*), (<*>), liftA2)
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.List as L


data Command = Rect (Int, Int)
             | RotateCol (Int, Int)
             | RotateRow (Int, Int) deriving (Show, Eq)

panelWidth = 50
panelHeight = 6

initMap :: (Int, Int) ->  M.Map (Int, Int) Bool
initMap (width, height) = M.fromAscList [((x, y), False)
                          | x <- [0..width-1]
                          , y <- [0..height-1]]

tup a b = (,) <$> a <*> b

commandParser :: Parser Command
commandParser = try rectParser <|>
                try rowParser  <|>
                try colParser where
  rectParser = Rect <$> (string "rect " >> (int <* char 'x') `tup` int)
  rowParser = RotateRow <$> (string "rotate row y=" >> (int <* string " by ") `tup` int)
  colParser = RotateCol <$> (string "rotate column x=" >> (int <* string " by ") `tup` int)


interpretCmd :: M.Map (Int, Int) Bool -> Command -> M.Map (Int, Int) Bool
interpretCmd m (Rect (width, height)) =
  M.fromAscList [((x, y), True)
    | x <- [0..width-1]
    , y <- [0..height-1]] <> m
interpretCmd m (RotateRow (y, amount)) =
  modRow amount (rowAt y m) <> m
interpretCmd m (RotateCol (x, amount)) =
  modCol amount (colAt x m) <> m

rowAt :: Int -> M.Map (Int, Int) Bool -> M.Map (Int, Int) Bool
rowAt y m = M.restrictKeys m rowSlice where
  rowSlice = Set.fromAscList [(x, y) | x <- [0..panelWidth]]
colAt :: Int -> M.Map (Int, Int) Bool -> M.Map (Int, Int) Bool
colAt x m = M.restrictKeys m colSlice where
  colSlice = Set.fromAscList [(x, y) | y <- [0..panelHeight]]

modRow :: Int ->  M.Map (Int, Int) Bool -> M.Map (Int, Int) Bool
modRow shiftAmt m = M.mapWithKey
  (\(ix, iy) _ -> m M.! ((ix - shiftAmt) `mod` panelWidth, iy)) m
modCol :: Int ->  M.Map (Int, Int) Bool -> M.Map (Int, Int) Bool
modCol shiftAmt m = M.mapWithKey
  (\(ix, iy) _ -> m M.! (ix, (iy - shiftAmt) `mod` panelHeight)) m


solution = do
  input <- lines <$> readFile "src/day8/input"
  let parsed = parse commandParser "" `traverse` input
  pure $ do
    commands <- parsed
    let emptyPanel = initMap (panelWidth , panelHeight )
    let part1 = M.size $ M.filter id $ foldl interpretCmd emptyPanel commands
    let part1Debug = printMap <$> scanl interpretCmd emptyPanel commands
    pure part1Debug


printMap m =
  let sortedList = L.sortOn (snd . fst) (M.toList m)
      groupedList =  L.groupBy (\((_, a), _) ((_, b), _) -> a == b) sortedList
      panel = fmap (fmap (\((_, _), bool) -> if bool then '#' else '.') ) groupedList
  in panel
