import Test.Hspec
import Lib
import Data

main :: IO ()

main =hspec $ do
    describe "formatGrid" $ do
        it "should concatenate every line with a newline" $ do
            formatGrid ["abc", "def", "ghi"]`shouldBe` "abc\ndef\nghi\n"

    describe "findWord" $ do
        it "should find words that exist in grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL"
            findWord grid "PERL" `shouldBe` Just "PERL"
        it "Should not find words that doesnt exist on the grid" $ do
            findWord grid "HAMSTER" `shouldBe` Nothing

    describe "findWords" $ do
        it "Should find all the words" $ do
            findWords grid languages `shouldBe` languages
