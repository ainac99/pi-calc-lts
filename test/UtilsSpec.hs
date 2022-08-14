module UtilsSpec (spec) where

import Test.Hspec
import Utils
import Syntax
import Pretty
import Language.Nominal

spec :: Spec
spec = do 
    describe "Utils.combine" $ do 
        it "Returns all pair combinations between elements of two lists" $ do
            Utils.combine [1, 2] [3, 4] `shouldMatchList` [(1,3), (1,4), (2,3), (2,4)]
        it "Returns an empty list if one of them is empty" $ do
            Utils.combine [] [1,2] `shouldBe` []
            Utils.combine [1,2] [] `shouldBe` []

    describe "Utils.mysub" $ do
        let     a = freshChannel "a"
                b = freshChannel "b"
                c = freshChannel "c"
        it "Capture-avoidingly substitutes in Out" $ do
            mysub a b (Out a c Nil) `shouldBe` Out b c Nil
            mysub a b (Out c a Nil) `shouldBe` Out c b Nil
            mysub a b (Out a a Nil) `shouldBe` Out b b Nil
            mysub a b (Out c c Nil) `shouldBe` Out c c Nil
            mysub a b (Out a c (Out a a Nil)) `shouldBe` Out b c (Out b b Nil)
            mysub a b (Out c a (rest a Nil)) `shouldBe`  Out c b (rest a Nil)
        it "Capture-avoidingly substitutes in In" $ do
            mysub a b (inp a a Nil) `shouldBe` inp b a Nil
            mysub a b (inp c c Nil) `shouldBe` inp c c Nil
            mysub a b (inp a c Nil) `shouldBe` inp b c Nil
            mysub a b (inp c a Nil) `shouldBe` inp c a Nil
            mysub a b (inp a c (inp a a Nil)) `shouldBe` inp b c (inp b a Nil)
        it "Capture-avoidingly substitutes in New" $ do
            mysub a b (rest a Nil) `shouldBe` rest a Nil
            mysub a b (rest c Nil) `shouldBe` rest c Nil
            mysub a b (rest c (rest a Nil)) `shouldBe` rest c (rest a Nil)
            mysub a b (rest c (rest c (Out a a Nil))) `shouldBe` rest c (rest c (Out b b Nil))
        it "Capture-avoidingly substitutes in Nil" $ do
            mysub a b Nil `shouldBe` Nil
        it "Capture-avoidingly substitutes in Par" $ do 
            mysub a b (Par (inp a a Nil)(Out a a Nil)) `shouldBe` Par (inp b a Nil)(Out b b Nil)
    
    describe "Utils.getChannel" $ do 
        let a = freshChannel "a"
        it "Retrieves the label of a bound name, and returns a fresh name with this label" $ do 
            nameLabel (getChannel (a @> Nil)) `shouldBe` "a"
            getChannel (a@> Nil) `shouldNotBe` a -- getChannel should return a fresh name
