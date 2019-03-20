{-# LANGUAGE LambdaCase #-}
module Day06_2018 where

import           Text.Parsec.String
import           Text.Parsec.Prim (parse)
import           Text.Parsec.Error (ParseError)
import           Text.ParserCombinators.Parsec.Number ( int )
import           Text.Parsec.Char
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import Data.Foldable
import Common

-- Coordinates are given as (Col, Row) !
type Point = (Integer, Integer)
type Quad = (Point, Point, Point, Point)
type Index = Integer

data Condition
  = Unsearched
  | Fault
  | HasCrystal Index deriving (Eq, Ord)

newtype SeedCrystal = SeedCrystal (Point, Index)

-- By the end, all points should be Just
-- growCrystals :: Map Point Condition -> Map Point Condition
-- growCrystals m = M.foldMapWithKey (\(py, px) a -> case a of
--     HasCrystal i -> let
--       adjPts = [(py, px-1),(py-1, px), (py, px+1), (py+1, px)]
--       expandedFront = adjPts `zip` repeat (HasCrystal i) |> M.fromList
--       in m <> expandedFront
--     Fault -> m
--     Unsearched -> m) m

growCrystals' :: Set Point -> Set Point ->
  Map Point Condition -> (Set Point, Map Point Condition)
growCrystals' searched crystalFront substrate = if crystalFront |> S.null
  then (crystalFront, substrate)
  -- Find the adjacent points to the front
  -- Resolve what grain they should contain based on adjacents
  -- Recurse on the new front and mconcat the substrate
  else let
    newFront = crystalFront |> S.map adjacents |> S.toList
      |> mconcat  |> filter (`S.notMember` crystalFront) |> S.fromList
    updatedMap = S.map (\p -> (p, substrate M.! p)) crystalFront |>
      S.toList |> M.fromList  
    in (newFront, undefined)

adjacents :: Point -> [Point]
adjacents (py, px) = [(py, px-1),(py-1, px), (py, px+1), (py+1, px)]

initializeSubstrate :: Integer -> Map Point Condition
initializeSubstrate i = let
  points = [0..i] `zip` [0..i]
  in M.fromList $ points `zip` repeat Unsearched


isFullyEnclosed :: Set Point -> Point -> Bool
isFullyEnclosed pts p =
  p |> isWithinQuad (largestEnclosingQuad pts)

isWithinQuad :: Quad -> Point -> Bool
isWithinQuad (tl, tr, bl, br) p =
  (tl |> isPositioned TopLeftOf  p == GT) &&
  (tr |> isPositioned TopRightOf p == GT) &&
  (bl |> isPositioned BotLeftOf  p == GT) &&
  (br |> isPositioned BotRightOf p == GT)


largestEnclosingQuad :: Set Point -> Quad
largestEnclosingQuad pts = let
  tl = maximumBy (isPositioned TopLeftOf) pts
  tr = maximumBy (isPositioned TopRightOf) pts
  bl = maximumBy (isPositioned BotLeftOf) pts
  br = maximumBy (isPositioned BotRightOf) pts
  in (tl, tr, bl, br)


data RelativeDirection
  = TopLeftOf
  | TopRightOf
  | BotLeftOf
  | BotRightOf

isPositioned :: RelativeDirection -> Point -> Point -> Ordering
isPositioned TopLeftOf (ay, ax) (by, bx)
  | ax == bx || ay == by = EQ
  | ax < bx && ay < by = GT
  | otherwise = LT
isPositioned TopRightOf (ay, ax) (by, bx)
  | ax == bx || ay == by = EQ
  | ax > bx && ay < by = GT
  | otherwise = LT
isPositioned BotLeftOf (ay, ax) (by, bx)
  | ax == bx || ay == by = EQ
  | ax < bx && ay > by = GT
  | otherwise = LT
isPositioned BotRightOf (ay, ax) (by, bx)
  | ax == bx || ay == by = EQ
  | ax > bx && ay > by = GT
  | otherwise = LT
