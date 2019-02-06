{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}


module Day10Lib0 where

import Text.Parsec (many1, try)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator (sepBy, between)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Char
import Control.Applicative ((<|>), (*>), (<*), (<*>), liftA2, many)
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.List as L
import qualified Data.Graph as G


tup a b = (,) <$> a <*> b

type MicrochipValue = Int
type Index = Int



data Subject = OutputSubject Index
             | BotSubject Index
             deriving (Eq, Show)
to (OutputSubject i) = i
to (BotSubject i) = i


newtype GoesToInstruction = GoesToInstruction (MicrochipValue, Index)
newtype GivesInstruction = GivesInstruction (Index, Subject, Subject)


goesToParser :: Parser GoesToInstruction
goesToParser = GoesToInstruction <$> ((string "value " >> int) <* string " goes to bot ") `tup` int

givesParser :: Parser GivesInstruction
givesParser = fmap (\((a, b), c) -> GivesInstruction (a,b,c))
              ((string "bot " *> int) `tup`
              (string " gives low to " *> subjectParser) `tup`
              (string " and high to " *> subjectParser))

subjectParser :: Parser Subject
subjectParser = BotSubject <$> (string "bot " *> int) <|>
                OutputSubject <$> (string "bot " *> int)


sampleGoes :: [GoesToInstruction]
sampleGoes = GoesToInstruction <$> [(5, 2), (3, 1), (2, 2)]

sampleGives :: [GivesInstruction]
sampleGives = GivesInstruction <$> [
  (2, BotSubject 1, BotSubject 0),
  (1, OutputSubject 1, BotSubject 0),
  (0, OutputSubject 2, OutputSubject 0 )]


class Edgeable a where
  toEdges :: a ->  [G.Edge]

instance Edgeable GoesToInstruction where
  toEdges (GoesToInstruction (v, i)) = [(-1, i)]

instance Edgeable GivesInstruction where
  toEdges (GivesInstruction (i, s1, s2)) = [(i, to s1), (i, to s2)]

girraffe = G.buildG (-1, 250) (mconcat (toEdges <$> sampleGoes)
                            <> mconcat (toEdges <$> sampleGives))
