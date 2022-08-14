{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Syntax
import Pretty()
import LTS
import Utils

import Prelude hiding ((||))
import Language.Nominal

main :: IO()
main = do
        printTransitions testProcesses 3 -- choose depth of evaluation here.

-- Given a list of processes, finds their transition tree up to a certain depth 
-- and prints them grouped by origin state.
printTransitions :: [Process] -> Integer -> IO()
printTransitions _ 0 = return()
printTransitions procList depth = do
                                        print nextLevel                                   
                                        printTransitions (newOrigins nextLevel) (depth-1) 
                                  where nextLevel = map transition procList              


-- Given a list of transitions grouped by origin state, 
-- returns the list of their destination states and unifies them in a single list.
newOrigins :: [[Transition]] -> [Process]
newOrigins = concatMap (map getDestState)


-- | List of processes to evaluate.
testProcesses :: [Process]
--testProcesses = [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14]
testProcesses = [t8]

-- | Some example processes
t1 = (\[x, y, z] -> Out x y (inp z y (Out y y Nil))) `genAppC` freshNames["x", "y", "z"] :: Process
t2 = (\[x, y] -> inp x x (Out y x Nil)) `genAppC` freshNames["x", "y"] :: Process
t3 = (\[x, y] -> inp x y (Out y y Nil)) `genAppC` freshNames["x", "y"] :: Process
t4 = (\[x, y, z] -> rest y (inp x z (Out z z (Out y z Nil)))) `genAppC` freshNames["x", "y", "z"] :: Process
t5 = (\[x, y, z, t] -> Par (Out x y (inp y t Nil))(inp x z (Out z z Nil))) `genAppC` freshNames["x", "y", "z", "t"] :: Process
t6 = (\[x, y, t, y'] -> Par (rest y (Out x y (inp y t Nil))) (inp x y' (Out y' y' Nil))) `genAppC` freshNames["x", "y", "t", "y'"] :: Process
t7 = (\[x, y, t] -> rest y (Out x y (inp y t Nil))) `genAppC` freshNames["x", "y", "t"] :: Process
t8 = (\[x, y, z] -> Par (inp x y (Out y y Nil)) (Out x z Nil)) `genAppC` freshNames["x", "y", "z"] :: Process
t9 = (\[x, y, z, t] -> rest y (Par (Out x y (inp y t Nil)) (inp x z (Out z z Nil)))) `genAppC` freshNames ["x", "y", "z", "t"] :: Process
t10 = (\[x, y, z, t] -> Par (inp x y (inp y t Nil)) (Out x z Nil)) `genAppC` freshNames["x", "y", "z", "t"] :: Process
t11 = (\[y,a] -> Par (Out y y Nil) (inp y a Nil)) `genAppC` freshNames["y", "a"] :: Process
t12 = (\[a,y,z] -> Par (Out a z Nil) (inp a y (Out y y Nil)))`genAppC` freshNames["a", "y", "z"] :: Process
t13 = (\[a, y, z] -> Par (Out a z Nil)(inp a y (rest y (Out y y Nil)))) `genAppC` freshNames["a", "y", "z"] :: Process
t14 = (\[a, b, y, z] -> Par (Out a z Nil)(inp a y (rest b (Out b y Nil)))) `genAppC` freshNames["a", "b", "y", "z"] :: Process