module Day1 where

import           Data.List
import           Data.Monoid
import           Data.List                      ( scanl )
import           Data.List.Split                (splitOn)
import           Text.Parsec.String
import           Text.Parsec.Prim
import           Text.Parsec.Char
import           Text.Parsec.Error (ParseError)
import           Text.ParserCombinators.Parsec.Number ( int )
import           Control.Monad.Trans.Either


someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Coord2D = C2D (Integer, Integer) deriving Show
data Compass = North | East | South | West deriving (Show, Eq, Enum, Ord, Bounded)
data Move = Move Direction Integer deriving (Show, Eq)
data Direction = R | L deriving (Show, Eq)

instance Monoid Coord2D where
    mempty = C2D (0,0)

instance Semigroup Coord2D where
    (<>) (C2D (ax, ay)) (C2D (bx, by)) = C2D ((ax + bx), (ay + by))



rParser :: Parser Move
rParser = do
  _ <- (char 'R')
  n <- int
  return $ Move R n

lParser :: Parser Move
lParser = do
  _ <- (char 'L')
  n <- int
  return $ Move L n

moveParser = rParser <|> lParser

orient :: (Compass, Integer) -> Move -> (Compass, Integer)
orient (compass, _) (Move R d) = (prev compass, d)
orient (compass, _) (Move L d) = (next compass, d)

ordinate :: (Compass, Integer) -> Coord2D
ordinate (_    , 0) = C2D (0, 0)
ordinate (North, d) = C2D (0, d)
ordinate (East , d) = C2D (d, 0)
ordinate (South, d) = C2D (0, -d)
ordinate (West , d) = C2D (-d, 0)




next :: (Enum a, Bounded a) => a -> a
next = turn 1
prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
  where add mod x y = (x + y + mod) `rem` mod


-- RUNNING STUFF IN IO
-- runDay1 :: EitherT ParseError IO String
runDay1Part1 =  do 
  moveStrings <-  (splitOn ", ") <$> (readFile "./test/day1/input" )
  parsedRes <-  pure $ (parse moveParser "") <$> moveStrings
  traversed <- pure $ sequence parsedRes
  coord2ds <- pure $ fmap (\x -> fmap ordinate (scanl orient (North, 0) x)) traversed
  finalCoord <- pure $ fmap mconcat coord2ds
  manhattanDist <- pure $ fmap manhattanDist finalCoord
  putStrLn $ show manhattanDist


manhattanDist :: Coord2D -> Integer 
manhattanDist (C2D (x, y)) = (abs x) + (abs y)