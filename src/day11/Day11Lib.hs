{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE NamedFieldPuns #-}

module Day11Lib where

import Text.Parsec (many1, try)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator (sepBy, between)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Char
import Control.Applicative ((<|>), (*>), (<*), (<*>), liftA2, many)
import Control.Arrow ((>>>), (***))
import Control.Monad (join)
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.List as L



tup a b = (,) <$> a <*> b

type Depth = Int

data Floor = F1 | F2 | F3 | F4 deriving (Eq, Ord, Enum, Show)

data ComponentType = M | G deriving (Show)
data Element = S | P | R | T | C deriving (Show)
data Component = Component Element ComponentType deriving (Show)

data Building = Building
  { sg :: Floor, sm :: Floor
  , pg :: Floor, pm :: Floor
  , rg :: Floor, rm :: Floor
  , tg :: Floor, tm :: Floor
  , cg :: Floor, cm :: Floor
  , e  :: Floor
  } deriving (Eq, Ord, Show)


finish = Building F4 F4 F4 F4 F4 F4 F4 F4 F4 F4 F4
targets = targetOn <$> [F1, F1, F2, F4]
targetOn elevator = Building
  { sg = F1, sm = F1, pg = F1, pm = F1
  , tg = F2, rg = F2, rm = F2, cg = F2, cm = F2
  , tm = F3
  , e = elevator
  }

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y:ys)
 | n < 0     = []
 | otherwise = case drop (n-1) xs of
                 [ ] -> []
                 [_] -> [xs]
                 _   -> [y:c | c <- combinations (n-1) ys]
                           ++ combinations n ys

uniq :: (Ord a) => [a] -> [a]
uniq = map head . L.group . L.sort

searchedStates :: Set.Set Building
searchedStates = mempty

-- Recursively search until :
--  State key is in Set of searched-states
--  State key matches target
search :: Building -> [Building] -> Set.Set Building -> [Building]-> [[Building]]
search start ends seenStates resultsAcc = let
  newMoves = filter (not . (`elem` seenStates)) $ generateMoves start
  updatedSeenStates = seenStates <> Set.fromList newMoves
  found = (\mv ->
              let x = if mv `elem` ends
                      then resultsAcc <> [mv]
                      else resultsAcc <> [mv]
                        -- mconcat (search mv ends updatedSeenStates (resultsAcc <> [mv]))
              in x
              ) <$> newMoves
  in found


-- Can't have: an unpaired microchip and unpaired generator on the same floor
validate :: Building -> Bool
validate (Building
   sg' sm'
   pg' pm'
   tg' tm'
   rg' rm'
   cg' cm'
   e' )
   = let
    evalMicro m = (m /= sg' || sm' == sg') && (m /= pg' || pg' == pm') &&
                  (m /= tg' || tg' == tm') && (m /= rg' || rg' == rm') &&
                  (m /= cg' || cg' == cm')
    in evalMicro sm' && evalMicro pm' && evalMicro tm' && evalMicro rm' && evalMicro cm'


flUp e = if e == F4
  then F4
  else succ e

flDn e = if e == F1
  then F1
  else pred e

-- Need to move 2 of the objects that are on
--   the same floor as the elevator
generateMoves :: Building -> [Building]
generateMoves b =  filter validate $
  uniq $ generateMovesFromComponents b $ findOnesOnSameFloor b


generateMovesFromComponents :: Building -> [Component] -> [Building]
generateMovesFromComponents _ [] = []
generateMovesFromComponents b [c1] =
  [ moveComponent c1 b $ flDn (e b)
  , moveComponent c1 b $ flUp (e b)
  ]
generateMovesFromComponents b [c1, c2] =
  [ moveComponent c2 (moveComponent c1 b $ flDn (e b)) $ flDn (e b)
  , moveComponent c2 (moveComponent c1 b $ flUp (e b)) $ flUp (e b)
  ] <> generateMovesFromComponents b [c1]
    <> generateMovesFromComponents b [c2]
generateMovesFromComponents b cs = let
  pairPermutations = combinations 2 cs
  recursiveSearch = fmap (generateMovesFromComponents b) pairPermutations
  in mconcat recursiveSearch

moveComponent :: Component -> Building -> Floor -> Building
moveComponent (Component S G) b fl = b {sg = fl, e = fl}
moveComponent (Component S M) b fl = b {sm = fl, e = fl}
moveComponent (Component P G) b fl = b {pg = fl, e = fl}
moveComponent (Component P M) b fl = b {pm = fl, e = fl}
moveComponent (Component T G) b fl = b {tg = fl, e = fl}
moveComponent (Component T M) b fl = b {tm = fl, e = fl}
moveComponent (Component R G) b fl = b {sg = fl, e = fl}
moveComponent (Component R M) b fl = b {sm = fl, e = fl}
moveComponent (Component C G) b fl = b {cg = fl, e = fl}
moveComponent (Component C M) b fl = b {cm = fl, e = fl}

findOnesOnSameFloor :: Building -> [Component]
findOnesOnSameFloor (Building sg' sm' pg' pm' tg' tm' rg' rm' cg' cm' e' )
   = snd <$> filter fst
      [ (sg' == e', Component S G), (sm'== e', Component S M)
      , (pg' == e', Component P G), (pm'== e', Component P M)
      , (tg' == e', Component T G), (tm'== e', Component T M)
      , (rg' == e', Component R G), (rm'== e', Component R M)
      , (cg' == e', Component C G), (cm'== e', Component C M)]
