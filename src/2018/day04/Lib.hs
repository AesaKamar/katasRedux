module Day04_2018 where

import           Text.Parsec.String
import           Text.Parsec.Prim (parse)
import           Text.Parsec.Error (ParseError)
import           Text.ParserCombinators.Parsec.Number ( int )
import Text.Parsec.Char

import Common
import Control.Applicative ((<|>), (*>), (<*), (<*>), liftA2)
import Data.List (sort)



data DateTime = Dt {year:: Integer, month :: Integer, day :: Integer, hour :: Integer, minute :: Integer}
  deriving (Show, Eq, Ord)

newtype Guard = Guard Integer deriving (Show, Eq)

data Command = Start Guard
             | Sleep
             | Wake
             deriving (Show, Eq)
instance Ord Command where
  compare a b = EQ

data LogEntry  = LogEntry DateTime Command deriving (Show, Eq, Ord)

logEntryParser :: Parser LogEntry
logEntryParser = do
  dt <- dateTimeParser <* char ' '
  cmd <- commandParser
  pure $ LogEntry dt cmd

dateTimeParser :: Parser DateTime
dateTimeParser = do
  year <- char '[' *> int
  month <- char '-' *> int
  day <- char '-' *> int
  hour <- char ' ' *> int
  minute <- char ':' *> int <* char ']'
  pure $ Dt year month day hour minute

commandParser :: Parser Command
commandParser =
  (string "Guard #" *> int <* string " begins shift") |$> (Start . Guard) <|>
  string "falls asleep" |$> const Sleep <|>
  string "wakes up" |$> const Wake


parseAndOrder :: String -> Either ParseError [LogEntry]
parseAndOrder s = lines s |$> parse logEntryParser "" |> sequence |$> sort
