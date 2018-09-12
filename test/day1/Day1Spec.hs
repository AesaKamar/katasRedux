module Day1 where 

import           Test.Tasty
import           Test.Tasty.Hspec
import           System.IO
import           Data.String
import           Data.List.Split

main :: IO ()
main = do
  fl <- (splitOn ", ") <$> readFile ("./test/day1/input") 
  putStrLn $ show fl


