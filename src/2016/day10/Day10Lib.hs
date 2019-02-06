{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}


module Day10Lib where

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

instance Show ((Index -> Node) -> (Index -> Node) -> Node) where
  show = const "λ"

instance Show (MicrochipValue -> (Index -> Node) -> (Index -> Node) -> Node) where
  show = const "λ"

instance Show (Index -> Node) where
  show = const "λ"

data Node = Output |
            Bot MicrochipValue MicrochipValue (Index -> Node) (Index -> Node)
            deriving (Show)

data UnfinishedBot = NeedsOne (MicrochipValue -> (Index -> Node) -> (Index -> Node) -> Node)
                   | Ready ((Index -> Node) -> (Index -> Node) -> Node)
                   deriving (Show)

goesToParser :: Parser (MicrochipValue, Index)
goesToParser = ((string "value " >> int) <* string " goes to bot ") `tup` int

data Subject = OutputSubject Index | BotSubject Index deriving (Eq,  Show)
to (OutputSubject i) = i
to (BotSubject i) = i


subjectParser :: Parser Subject
subjectParser = BotSubject <$> (string "bot " *> int) <|>
                OutputSubject <$> (string "bot " *> int)


givesParser :: Parser (Index, Subject, Subject)
givesParser = fmap (\((a, b), c) -> (a,b,c))
              ((string "bot " *> int) `tup`
              (string " gives low to " *> subjectParser) `tup`
              (string " and high to " *> subjectParser))


applyGoes ::  M.Map Index UnfinishedBot ->  (MicrochipValue, Index)  -> M.Map Index UnfinishedBot
applyGoes m (v, k) = case m M.!? k of
    Nothing -> M.insert k (NeedsOne (Bot v)) m
    Just (NeedsOne f) -> M.update (const $ Just $ Ready (f v)) k m

applyGives :: (M.Map Index UnfinishedBot, M.Map Index Node)
           -> (Index, Subject, Subject)
           -> (M.Map Index UnfinishedBot, M.Map Index Node)
applyGives (m, n) (i, low, high) =
  case m M.!? i of
    Just (Ready f)  ->
      let node = f (n M.!) (n M.!)
      in (M.delete i m, M.insert i node n)
    Nothing -> undefined

sampleGoes :: [(MicrochipValue, Index)]
sampleGoes = [(5, 2), (3, 1), (2, 2)]

sampleGives :: [(Index, Subject, Subject)]
sampleGives = [
  (2, BotSubject 1, BotSubject 0),
  (1, OutputSubject 1, BotSubject 0),
  (0, OutputSubject 2, OutputSubject 0 )]

holdingMap = F.foldl applyGoes mempty sampleGoes
instructionsMap = F.foldl applyGives mempty sampleGives

-- doStuff :: [(MicrochipValue, Index)] -> [(Index, Subject, Subject)]
--         -> M.Map Index UnfinishedBot -> M.Map Index Node
--         -> (M.Map Index UnfinishedBot, M.Map Index Node)
-- doStuff goes gives unfMap nodeMap =
--   (F.foldl applyGoes mempty undefined,
--   F.foldl applyGives mempty undefined)

-- collapse :: M.Map Index UnfinishedBot ->
