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

instance Show (Maybe MicrochipValue -> Node) where
  show = const "λ"
instance Show (Maybe MicrochipValue -> Maybe MicrochipValue -> Node) where
  show = const "2λ"

data Node = Output |
            Bot (Maybe MicrochipValue) (Maybe MicrochipValue)
            deriving (Show, Eq)

data UnfinishedBot = NeedsTwo Node
                   | NeedsOne Node
                   | Ready Node
                   deriving (Show)

data Subject = OutputSubject Index
             | BotSubject Index
             deriving (Eq, Show)
to (OutputSubject i) = i
to (BotSubject i) = i


newtype GoesToInstruction = GoesToInstruction (MicrochipValue, Index)
newtype GivesInstruction  = GivesInstruction  (Index, Subject, Subject)


goesToParser :: Parser GoesToInstruction
goesToParser = GoesToInstruction <$> ((string "value " >> int) <* string " goes to bot ") `tup` int

givesParser :: Parser GivesInstruction
givesParser = fmap (\((a, b), c) -> GivesInstruction (a,b,c))
              ((string "bot " *> int) `tup`
              (string " gives low to " *> subjectParser) `tup`
              (string " and high to " *> subjectParser))

subjectParser :: Parser Subject
subjectParser = BotSubject    <$> (string "bot " *> int) <|>
                OutputSubject <$> (string "bot " *> int)


sampleGoes :: [GoesToInstruction]
sampleGoes = GoesToInstruction <$> [(5, 2), (3, 1), (2, 2)]

sampleGives :: [GivesInstruction]
sampleGives = GivesInstruction <$> [
  (2, BotSubject 1, BotSubject 0),
  (1, OutputSubject 1, BotSubject 0),
  (0, OutputSubject 2, OutputSubject 0 )]

something1 :: M.Map Index UnfinishedBot
something1 = F.foldl updateMapWithGoes mempty sampleGoes
something2 = F.foldl updateMapWithGives something1 sampleGives

updateMapWithGoes ::  M.Map Index UnfinishedBot -> GoesToInstruction -> M.Map Index UnfinishedBot
updateMapWithGoes m (GoesToInstruction (v, i)) = case m M.!? i of
  Nothing -> M.insert i  (NeedsOne (Bot (Just v) Nothing )) m
  Just (NeedsOne (Bot l r )) -> M.insert i (Ready (Bot l (Just v))) m
  otherwise -> error "Oops, found something wrong [updateMapWithGoes]"

updateMapWithGives :: M.Map Index UnfinishedBot -> GivesInstruction -> M.Map Index UnfinishedBot
updateMapWithGives m (GivesInstruction (i, BotSubject s1, BotSubject s2)) = case m M.!? i of
  Just (Ready (Bot (Just v1) (Just v2))) -> let
    low = min v1 v2
    high = max v1 v2
    in undefined
  Just (NeedsOne (Bot l _)) -> error "Oops, only 1 to give"
  Just (Ready (Bot _ _)) -> error "Oops, nothing to give"
  otherwise -> error ("[updateMapWithGives] " <> show otherwise <> " " <> show m )


giveTo :: M.Map Index UnfinishedBot -> Index -> MicrochipValue ->  M.Map Index UnfinishedBot
giveTo m i v = case m M.!? i of
  Nothing -> NeedsOne
