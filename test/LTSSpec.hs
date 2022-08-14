module LTSSpec (spec) where

import Test.Hspec
import LTS
import Syntax
import Language.Nominal
import Utils

p1 = (\[x, y, z] -> Out x y (inp z y (Out y y Nil))) `genAppC` freshNames["x", "y", "z"] :: Process
p2 = (\[x, y] -> inp x x (Out y x Nil)) `genAppC` freshNames["x", "y"] :: Process
p3 = (\[x, y, z] -> rest y (Out x y (inp y z (Out z z Nil)))) `genAppC` freshNames["x", "y", "z"] :: Process
p4 = (\[x, y, z] -> rest y (inp x z (Out z z (Out y z Nil)))) `genAppC` freshNames["x", "y", "z"] :: Process
p5 = (\[x, y, z, t] -> Par (Out x y (inp y t Nil))(inp x z (Out z z Nil))) `genAppC` freshNames["x", "y", "z", "t"] :: Process
p6 = (\[x, y, t, y'] -> Par (rest y (Out x y (inp y t Nil))) (inp x y' (Out y' y' Nil))) `genAppC` freshNames["x", "y", "t", "y'"] :: Process
p7 = (\[x, y, t] -> rest y (Out x y (inp y t Nil))) `genAppC` freshNames["x", "y", "t"] :: Process

spec :: Spec
spec = do
    describe "test1 - Out rule" $ do
            let ts = transition p1
                (p, z, a, q, r) = unpack (head ts)
                tr1 = (\[y,z] -> inp z y (Out y y Nil)) `genAppC` freshNames["y", "z"]
            
            it "length of transition should be 1"        $ do length ts `shouldBe` 1
            it "should match origin process"             $ do p1 `shouldBe` p
            it "should be Out rule"                      $ do r `shouldMatchList` [Out_r]
            it "show destination process"                $ do show q `shouldBe` "z(y).y<y>.0"
            it "show action"                             $ do show a `shouldBe` "x<y>"
            it "action should not contain bound channel" $ do arguments_act a `shouldNotContain` [z]

    describe "test2 - In rule" $ do
            let ts = transition p2
                (p, z, a, q, r) = unpack (head ts)
                tr1 = (\[y,x] -> Out y x Nil) `genAppC` freshNames["y", "x"]

            it "length of transition should be 1"   $ do length ts `shouldBe` 1
            it "should match origin process"        $ do p2 `shouldBe` p
            it "should be In rule"                  $ do r `shouldMatchList` [In_r]
            it "show destination process"           $ do show q `shouldBe` show tr1
            it "show action"                        $ do show a `shouldBe` "x(x)"
            it "bound action should be z.x(z)"      $ do arguments_act a !! 1 `shouldBe` z
    
    describe "test3 - Open rule" $ do
            let ts = transition p3
                (p, z, a, q, r) = unpack (head ts)
                tr1 = (\[y,z] -> inp y z (Out z z Nil)) `genAppC` freshNames["y", "z"]

            it "length of transition should be 1"   $ do length ts `shouldBe` 1
            it "should match origin process"        $ do p3 `shouldBe` p
            it "should be Open rule"                $ do r `shouldMatchList` [Open_r]
            it "show destination process"           $ do show q `shouldBe` show tr1
            it "show action"                        $ do show a `shouldBe` "x<y>"
            it "bound action should be z.y<z>"      $ do arguments_act a !! 1 `shouldBe` z

    describe "test4 - Res rule" $ do
            let ts = transition p4
                (p, z, a, q, r) = unpack (head ts)
                tr1 = (\[y,z] -> rest y (Out z z (Out y z Nil))) `genAppC` freshNames["y", "z"]

            it "length of transition should be 1"   $ do length ts `shouldBe` 1
            it "should match origin process"        $ do p4 `shouldBe` p
            it "should be Res rule"                 $ do r `shouldMatchList` [Res_r, In_r]
            it "show destination process"           $ do show q `shouldBe` show tr1
            it "show action"                        $ do show a `shouldBe` "x(z)"
            it "bound action should be z.y<z>"      $ do arguments_act a !! 1 `shouldBe` z 

    describe "test5 - Par, Com, In, Out rules" $ do
            let ts = transition p5
                tr1 = (\[y, t, x, z] -> Par (inp y t Nil)(inp x z (Out z z Nil)))   `genAppC` freshNames ["y", "t", "x", "z"]
                tr2 = (\[z, x, y, t] -> Par (Out z z Nil)(Out x y (inp y t Nil)))   `genAppC` freshNames ["z", "x", "y", "t"]
                tr3 = (\[y, t] -> Par (inp y t Nil)(Out y y Nil))                   `genAppC` freshNames ["y", "t"]

            it "length of transition should be 3"   $ do length ts `shouldBe` 3
            it "should contain Out Par transition"  $ do checkContains p5 [Out_r, Par_r] "z.x<y>" tr1 ts   `shouldBe` True
            it "should contain In Par transition"   $ do checkContains p5 [In_r, Par_r] "z.x(z)" tr2 ts    `shouldBe` True
            it "should contain Com transition"      $ do checkContains p5 [Com_r] "z.τ" tr3 ts             `shouldBe` True

    describe "test6 - Open, Close, Par rules" $ do
            let ts = transition p6
                tr1 = (\[x, y, t, y'] -> Par (inp y t Nil)(inp x y' (Out y' y' Nil)))           `genAppC` freshNames ["x", "y", "t", "y'"]
                tr2 = (\[x, y, t, y'] -> Par (Out y' y' Nil)(rest y (Out x y (inp y t Nil))))   `genAppC` freshNames ["x", "y", "t", "y'"]
                tr3 = (\[y, t] -> rest y (Par (inp y t Nil)(Out y y Nil)))                      `genAppC` freshNames ["y", "t"]

            it "length of transition should be 3"    $ do length ts `shouldBe` 3
            it "should contain Open Par transition"  $ do checkContains p6 [Open_r, Par_r] "y.x<y>" tr1 ts `shouldBe` True
            it "should contain In Par transition"    $ do checkContains p6 [In_r, Par_r] "y'.x(y')" tr2 ts `shouldBe` True
            it "should contain Close transition"     $ do checkContains p6 [Close_r] "y.τ" tr3 ts          `shouldBe` True

    describe "test7 - Open rule" $ do
                let ts = transition p7
                    (p, z, a, q, r) = unpack (head ts)
                    tr1 = (\[y, t] -> inp y t Nil)   `genAppC` freshNames ["y", "t"]

                it "length of transition should be 1"   $ do length ts `shouldBe` 1
                it "should match origin process"        $ do p7 `shouldBe` p
                it "should be Open rule"                $ do r `shouldMatchList` [Open_r]
                it "show destination process"           $ do show q `shouldBe` show tr1
                it "show action"                        $ do show a `shouldBe` "x<y>"
                it "bound action should be y.x<y>"      $ do arguments_act a !! 1 `shouldBe` z 


checkContains p r a q ts =   (("\n" ++ show p ++ " \t --- " ++ show r ++ " " ++ a ++ " --> \t" ++ show q ++ "\n") `elem` map show ts )
                                Prelude.|| 
                                (("\n" ++ show (symmPar p) ++ " \t --- " ++ show r ++ " " ++ a ++ " --> \t" ++ show q ++ "\n") `elem` map show ts)

symmPar (Par p1 p2) = Par p2 p1
symmPar x = x