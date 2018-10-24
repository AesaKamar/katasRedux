module Day6Lib where

import Data.Map.Lazy hiding (take, foldl, filter, showTree)
import Data.List (sort, sortBy)
import Data.Map.Internal.Debug

type Index = Int
type Count = Int

updateMap :: Map Index (Map Char Count) ->  String -> Map Index (Map Char Count)
updateMap oldMap s =
  unionWith (unionWith (+)) oldMap (mapOccurences s)


mapOccurences :: String -> Map Index (Map Char Count)
mapOccurences s = let
  indexed =  fromList $ take (length s) ([0..] `zip` s)
  occurenceMap =  (\c -> singleton c 1 ) <$> indexed
  in occurenceMap

solution :: IO (Map Index [(Char, Count)])
solution = do
  input <- lines <$> readFile "src/day6/input"
  let result = foldl updateMap mempty input
  pure $
   (sortBy compareSnd . toList) <$> result


compareSnd :: Ord b =>  (a, b) -> (a, b) -> Ordering
compareSnd (_, i1) (_, i2) = compare i1 i2
