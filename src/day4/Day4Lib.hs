module Day4Lib where

import Text.Parsec (many, many1)
import Text.Parsec.Combinator (sepBy)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Char
import Control.Monad.Trans.Either
import qualified Data.Map.Lazy as Map
import Data.Monoid (mconcat)
import Data.List (sortBy, sort, group, intercalate, find)

data Room = Room { letters  :: [[Char]]
                 , sectorId :: Int
                 , checkSum :: [Char]
                 } deriving (Show, Eq)

roomParser :: Parser Room
roomParser = do
  l <- takeWhile (/= "") <$> many letter `sepBy` char '-'
  n <- int
  _ <- char '['
  c <- many1 letter
  _ <- char ']'
  pure Room {letters = l, sectorId = n, checkSum = c}

sortByLengthThenLexicographic :: String -> String -> Ordering
sortByLengthThenLexicographic a b = if length a == length b
  then compare b a
  else compare (length a) (length b)


alphabet = ['a'..'z']

verify :: Room -> Bool
verify r =
  let allLetterGroups = group $ sort $ mconcat $ letters r
      lettersByFrequency = sortBy sortByLengthThenLexicographic allLetterGroups
      firstLettersReversed = reverse $ head <$> lettersByFrequency
  in take 5 firstLettersReversed == checkSum r

decrypt :: Room -> [String]
decrypt r = let
  encryptedSentence = letters r
  shiftAmount = sectorId r `mod` length alphabet
  cypherAlphabet =  take (length alphabet) $ drop shiftAmount $ cycle alphabet
  mapping = Map.fromList $ alphabet `zip` cypherAlphabet
  decryptedSentence = do
    encryptedWord <- encryptedSentence
    pure $ (mapping Map.!) <$> encryptedWord
  in decryptedSentence

solution = do
  inString <- lines <$> readFile "src/day4/input"
  something <- pure $ do
    rooms <- parse roomParser ""  `traverse` inString
    sumOfVerified <- pure $ sum $ sectorId <$> filter verify rooms
    pure  sumOfVerified
  print something


solution2 = do
  inString <- lines <$> readFile "src/day4/input"
  something <- pure $ do
    rooms <- parse roomParser ""  `traverse` inString
    decryptedRooms <- pure $ (decrypt <$> rooms) `zip` (sectorId <$> rooms)
    pure $ find (\x -> fst x == ["northpole", "object", "storage"]) decryptedRooms
  print something
