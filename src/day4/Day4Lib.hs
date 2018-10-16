module Day4Lib where

import Text.Parsec (many, many1)
import Text.Parsec.Combinator (sepBy, between)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Char
import Control.Monad.Trans.Either
import qualified Data.Map.Lazy as Map
import Data.Monoid (mconcat)
import Data.List (sortBy, sort, group, intercalate, find)

data Room = Room { letters  :: [String]
                 , sectorId :: Int
                 , checkSum :: [Char]
                 } deriving (Show, Eq)

roomParser :: Parser Room
roomParser = do
  l <-  many letter `sepBy` char '-'
  n <- int
  c <- between (char '[') (char ']') (many1 letter)
  pure Room {letters = init l, sectorId = n, checkSum = c}

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
  in take (length $ checkSum r) firstLettersReversed == checkSum r

decrypt :: Room -> [String]
decrypt r = let
  n = length alphabet
  shiftAmount = sectorId r `mod` n
  cypherAlphabet =  take n $ drop shiftAmount $ cycle alphabet
  mapping = Map.fromList (alphabet `zip` cypherAlphabet)
  decryptedSentence = do
    encryptedWord <- letters r
    pure $ (mapping Map.!) <$> encryptedWord
  in decryptedSentence

solution = do
  inString <- lines <$> readFile "src/day4/input"
  something <- pure $ do
    rooms <- parse roomParser "" `traverse` inString
    let verifiedRooms = filter verify rooms
    let part1 = sum $ sectorId <$> verifiedRooms
    let decryptedRooms = (decrypt <$> rooms) `zip` (sectorId <$> rooms)
    let part2 = find ((["northpole", "object", "storage"] ==) . fst) decryptedRooms
    pure  part1
  print something
