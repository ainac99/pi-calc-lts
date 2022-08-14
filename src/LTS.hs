{-# LANGUAGE DeriveGeneric         
           , MultiParamTypeClasses 
           , DeriveAnyClass       -- to derive 'Swappable' 
           , DeriveDataTypeable   -- to derive 'Data' 
           , FlexibleInstances     
#-}


module LTS where

import Syntax
import Utils
import Data.Generics hiding (Generic, typeof)
import GHC.Generics
import Prelude hiding ((||))
import Language.Nominal
import Data.Tuple
import Pretty

{- | Given a state returns the list of all its possible transitions.
     Transitions are found using the LTS presented in The pi-calculus in FM (Gabbay, 2003).
-}
transition :: State -> [Transition]

-- | OUT rule
transition (Out x y p) = [(Out x y p, abst z (Out_act x y, p), [Out_r])]
    where z = freshChannel "z"

-- | IN rule
transition (In x abstraction)
    | y `freshFor` x = [(In x abstraction, abst y (In_act x y, p), [In_r])]
    where   y = getChannel abstraction
            p = getBody y abstraction

-- | PAR & CLOSE & COM rules
transition (Par p1 p2) =    par p1 t1 p2 (l1 -1) ++                     -- par rule
                            par p2 t2 p1 (l2 -1) ++                     -- simmetric for par rule
                            checkCloseCom (t1 `combine` t2) (l1*l2 -1)  -- close & com rules
    where   t1 = transition p1
            t2 = transition p2
            l1 = length t1
            l2 = length t2

-- | OPEN & RES rules
transition (New abstraction) = open y t (l-1) ++ LTS.res y t (l-1)
    where   y = getChannel abstraction
            p = getBody y abstraction
            t = transition p
            l = length t

transition _ = []

-- | OPEN rule
open :: Channel -> [Transition] -> Int -> [Transition]
open _ _ (-1)   = empty
open y list gas
    | isOut_act a && y' `freshFor` x && y == y'&& z `freshFor` (p,q,x,y)
                        = (rest y p, abst y (Out_act x y, q), [Open_r]) : open y list (gas-1)
    | otherwise         = open y list (gas-1)
        where   t = list !! gas
                (p, z, a, q, r) = unpack t
                [x, y'] = arguments_act a

-- | RES rule
res :: Channel -> [Transition] -> Int -> [Transition]
res _ _ (-1)    = empty
res y list gas
    | not (isTau_act a) && y `freshFor` (z, x_act, y_act) = (rest y p, abst z (a, rest y q), r ++ [Res_r]) : LTS.res y list (gas-1)  -- y should be fresh for a too! (?)
    | otherwise                  = LTS.res y list (gas-1)
        where   t = list !! gas
                (p, z, a, q, r) = unpack t
                [x_act, y_act] = arguments_act a


-- | PAR rule
par :: Process -> [Transition] -> Process -> Int -> [Transition]
par _ _ _ (-1)   = empty
par p1 t1 p2 gas
    | z `freshFor` (p1, p2) = (Par p1 p2, abst z (a, Par q1 p2), r ++ [Par_r]) : par p1 t1 p2 (gas-1)
    | otherwise             = par p1 t1 p2 (gas-1)
    where   t = t1 !! gas
            (p1, z, a, q1, r) = unpack t

-- | Given a pair of transitions, checks whether close and com rules are applicable.
checkCloseCom :: [(Transition, Transition)] -> Int -> [Transition]
checkCloseCom _ (-1)    = empty
checkCloseCom lst gas   = close t  ++ close (swap t) ++ com t ++ com (swap t) ++ checkCloseCom lst (gas-1)
    where t = lst !! gas

-- | CLOSE rule
close :: (Transition, Transition) -> [Transition]
close (t1, t2)
    | isOut_act a1 && isIn_act a2 && x == x' && y `freshFor` (x, p1, p2) && head r1 == Open_r && head r2 == In_r
                     = [(p1 || p2, abst y (Tau_act, rest y (q1 || mysub y' y q2)), [Close_r])]
    | otherwise      = empty
    where   (p1, c1, a1, q1, r1) = unpack t1
            (p2, c2, a2, q2, r2) = unpack t2
            [x, y]               = arguments_act a1
            [x', y']             = arguments_act a2

-- | COM rule
com :: (Transition, Transition) -> [Transition]
com (t1, t2)
    | isOut_act a1 && isIn_act a2 && z == z2 &&  x == x' &&
        z1 `freshFor` (q1, p1, p2, y, x) && 
        z2 `freshFor` (q1, p1, p2, y, x)
                = [(p1 || p2, abst z2 (Tau_act, Par q1 (mysub z2 y q2)), [Com_r])]
    | otherwise = []
    where   (p1, z1, a1, q1, r1) = unpack t1
            (p2, z2, a2, q2, r2) = unpack t2
            [x, y]           = arguments_act a1
            [x', z]          = arguments_act a2

