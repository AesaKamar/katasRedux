{-# LANGUAGE LambdaCase #-}
module Day2Lib where

import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Char (char)
import Control.Applicative ((<|>), many)
import Control.Monad.Trans.Either


data SquareButton  =
   B1 | B2 | B3
 | B4 | B5 | B6
 | B7 | B8 | B9  deriving (Show, Eq)

data DiamondButton  =
          D1|
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


moveSquare :: Direction -> SquareButton -> SquareButton
moveSquare U = \case {
      B1->B1;B2->B2;B3->B3
    ; B4->B1;B5->B2;B6->B3
    ; B7->B4;B8->B5;B9->B6
}
moveSquare L = \case {
      B1->B1;B2->B1;B3->B2
    ; B4->B4;B5->B4;B6->B5
    ; B7->B7;B8->B7;B9->B8
}
moveSquare D = \case {
      B1->B4;B2->B5;B3->B6
    ; B4->B7;B5->B8;B6->B9
    ; B7->B7;B8->B8;B9->B9
}
moveSquare R = \case {
      B1->B2;B2->B3;B3->B3
    ; B4->B5;B5->B6;B6->B6
    ; B7->B8;B8->B9;B9->B9
}


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

moveSquareFlip :: SquareButton -> Direction -> SquareButton
moveSquareFlip = flip moveSquare

moveDiamondFlip :: DiamondButton -> Direction -> DiamondButton
moveDiamondFlip = flip moveDiamond

eachCharInLines :: String -> [[String]]
eachCharInLines = (fmap (\x -> [x]) <$>) . lines

parseTraverse :: [[String]] -> Either ParseError [[Direction]]
parseTraverse = traverse (traverse (parse directionP ""))


solution :: a -> (a -> Direction -> a)  ->  IO (Either ParseError [a])
solution startA moveFn= do
  eachLine <- eachCharInLines <$> readFile "./src/day2/input"
  parsedDirs <- pure $ parseTraverse eachLine
  pure $ do
    directions <- parsedDirs
    Right  $ (scanl $ foldl moveFn) startA directions
