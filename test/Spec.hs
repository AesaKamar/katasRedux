import           Test.Tasty
import           Test.Tasty.Hspec
import           System.IO
import           Data.String

main :: IO ()
main = hspec $ do
  describe "DayX" $ do
    it "part1"
      $ let fl = (\x -> x) <$> readFile ("./test/dayx/input") in 1 `shouldBe` 1

