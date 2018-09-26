{-# LANGUAGE LambdaCase #-}
module Day2Lib where

import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Char (char)
import Control.Applicative ((<|>), many)
import Control.Monad.Trans.Either


data SquareButton  = B1 | B2 | B3
                   | B4 | B5 | B6
                   | B7 | B8 | B9  deriving (Show, Eq)

data DiamondButton  =     D1|
                      D2| D3| D4|
                  D5| D6| D7| D8| D9|
                      DA| DB| DC|
                          DD       deriving (Show, Eq)

data Direction = U |
               L | R
               | D    deriving (Show, Eq)

directionP :: Parser Direction
directionP =
  pure U <$> char 'U' <|>
  pure L <$> char 'L' <|>
  pure D <$> char 'D' <|>
  pure R <$> char 'R'

directionsP :: Parser [Direction]
directionsP = many directionP

move :: SquareButton -> Direction -> SquareButton
move B1 = \case
  U -> B1
  L -> B1
  D -> B4
  R -> B2
move B2 = \case
  U -> B2
  L -> B1
  D -> B5
  R -> B3
move B3 = \case
  U -> B3
  L -> B2
  D -> B6
  R -> B3
move B4 = \case
  U -> B1
  L -> B4
  D -> B7
  R -> B5
move B5 = \case
  U -> B2
  L -> B4
  D -> B8
  R -> B6
move B6 = \case
  U -> B3
  L -> B5
  D -> B9
  R -> B6
move B7 = \case
  U -> B4
  L -> B7
  D -> B7
  R -> B8
move B8 = \case
  U -> B5
  L -> B7
  D -> B8
  R -> B9
move B9 = \case
  U -> B6
  L -> B8
  D -> B9
  R -> B9

moveDiamond :: Direction -> DiamondButton -> DiamondButton
moveDiamond U = \case {
                D1->D1;
         D2->D2;D3->D1;D4->D4;
  D5->D5;D6->D2;D7->D3;D8->D4;D9->D9;
         DA->D6;DB->D7;DC->D8;
                DD->DB
    }
moveDiamond L = \case {
                D1->D1;
         D2->D2;D3->D2;D4->D3;
  D5->D5;D6->D5;D7->D6;D8->D7;D9->D8;
         DA->DA;DB->DA;DC->DB;
                DD->DD
    }
moveDiamond D = \case {
                D1->D3;
         D2->D6;D3->D7;D4->D8;
  D5->D5;D6->DA;D7->DB;D8->DC;D9->D9;
         DA->DA;DB->DD;DC->DC;
                DD->DD
    }
moveDiamond R = \case {
                D1->D1;
         D2->D3;D3->D4;D4->D4;
  D5->D6;D6->D7;D7->D8;D8->D9;D9->D9;
         DA->DB;DB->DC;DC->DC;
                DD->DD
    }

moveDiamondFlip :: DiamondButton -> Direction -> DiamondButton
moveDiamondFlip = flip moveDiamond

eachCharInLines :: String -> [[String]]
eachCharInLines = (fmap (\x -> [x]) <$>) . lines

parseTraverse :: [[String]] -> Either ParseError [[Direction]]
parseTraverse = traverse (traverse (parse directionP ""))


x :: IO (Either ParseError [SquareButton])
x = do
  eachLine <- eachCharInLines <$> readFile "./src/day2/input"
  parsedDirs <- pure $ parseTraverse eachLine
  pure $ do
    directions <- parsedDirs
    Right  $ (scanl $ foldl move) B5 directions


y :: IO (Either ParseError [DiamondButton])
y = do
  eachLine <- eachCharInLines <$> readFile "./src/day2/input"
  parsedDirs <- pure $ parseTraverse eachLine
  pure $ do
    directions <- parsedDirs
    Right  $ (scanl $ foldl moveDiamondFlip) D5 directions
