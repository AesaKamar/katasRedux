{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Day9Lib where

import Text.Parsec (many1, try)
import Text.Parsec.Combinator (sepBy, between)
import Text.ParserCombinators.Parsec.Number (int)
import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Char
import Control.Applicative ((<|>), (*>), (<*), (<*>), liftA2)
import qualified Data.Map as M
import qualified Data.Foldable as F
import qualified Data.Set as Set
import qualified Data.List as L
