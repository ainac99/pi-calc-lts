{-# LANGUAGE BlockArguments #-}
module PrettySpec (spec) where

import Test.Hspec
import Pretty
import Utils
import Syntax
import Language.Nominal

spec :: Spec
spec =  do 
            describe "Pretty.ShowS" $ do 
                let a = freshChannel "a"
                if debugging 
                then 
                    it "ShowS (channel) Debugging: It shows both the label and unique id of a name" $ do
                        showS a `shouldBe`    (show (nameLabel a) ++ show (nameAtom a))
                else 
                    it "ShowS (channel) Not debugging: It shows only the label." $ do
                        showS a `shouldBe` "a"

            describe "Show instances" $ do
                let x = freshChannel "x"
                    y = freshChannel "y"
                    z = freshChannel "z"
                    t = (Out x y (inp z y (Out y y Nil)), abst z (Out_act x y, inp z y (Out y y Nil)), [Out_r])
                    
                it "show Actions" $ do
                    show Tau_act        `shouldBe` "τ"
                    show (In_act x y)   `shouldBe` "x(y)"
                    show (Out_act x y)  `shouldBe` "x<y>"
                it "show Process" $ do
                    show Nil            `shouldBe` "0"
                    show (inp x y Nil)  `shouldBe` "x(y).0"
                    show (Out x y Nil)  `shouldBe` "x<y>.0"
                    show (rest x Nil)   `shouldBe` "(ν(x) (0))"
                    show (Par Nil Nil)  `shouldBe` "0 | 0"
                it "show Transition" $ do
                    show t `shouldBe` "\nx<y>.z(y).y<y>.0 \t --- [Out_r] z.x<y> --> \tz(y).y<y>.0\n"  
