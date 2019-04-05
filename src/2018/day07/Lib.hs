{-# LANGUAGE LambdaCase #-}
module Day07_2018 where

import Algebra.Graph
import Algebra.Graph.ToGraph
import Text.Parsec.String
import Text.Parsec.Prim (parse)
import Text.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec.Number ( int )
import Text.Parsec.Char
import Text.Parsec.Combinator
import Common

letters = ['A'..'Z']

parser :: Parser (Graph Char)
parser =
  (string "Step " *> oneOf letters <* string " must be finished before step " ) `tup`
  (oneOf letters <* string " can begin.") $>
  (\(i,o) -> Connect (Vertex i) (Vertex o))


solution = do
  inp <- readFile "./src/2018/day07/input" $> lines  $> traverse (parse parser "")
  let res = do
        disconnectedEdges <- inp
        let connectedGraph = simplify <| overlays disconnectedEdges
        let topSorted = topSort connectedGraph
        let forst = dfsForest connectedGraph
        pure topSorted
  pure res
