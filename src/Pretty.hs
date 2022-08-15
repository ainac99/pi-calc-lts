{-
Module      : Pretty
Description : Pretty Printer
Copyright   : (c) Aina Centelles Tarrés, 2022
License     : MIT
Maintainer  : ainac99@gmail.com
-}

{-# LANGUAGE    FlexibleInstances,
                InstanceSigs #-}
module Pretty where

import Syntax
import Utils
import Language.Nominal

debugging = False :: Bool 

instance Show Process where
        show :: Process -> String
        show Nil             = "0"
        show (In c ab)       = showS c ++ "(" ++ showS bound ++ ")." ++ show body
                where   bound = getChannel ab
                        body  = getBody bound ab 
        show (Out c y p)    = showS c ++ "<" ++ showS y ++ ">." ++ show p
        show (New n)        = "(ν(" ++ showS bound ++ ") (" ++ show body ++ "))"
                where   bound = getChannel n
                        body  = getBody bound n 
        show (Par p q)      = show p ++ " | " ++ show q

instance {-# OVERLAPPING #-} Show Transition where
        show :: Transition -> String
        show t = "\n" ++ show p ++ " \t --- " ++ show r ++ " " ++ showS z ++ "." ++ show a ++ " --> \t" ++ show q ++ "\n"
            where  (p, z, a, q, r) = unpack t

instance Show Action where
        show :: Action -> String
        show Tau_act         = "τ"
        show (In_act x y)   = showS x ++ "(" ++ showS y ++ ")"
        show (Out_act x y)  = showS x ++ "<" ++ showS y ++ ">"

-- The unique identifier of a name is shown if debugging is True.
showS :: Channel -> String
showS (Name t s) = if   debugging 
                   then show (Name t s)
                   else nameLabel (Name t s)

-- Debug for substitution
debugSubs :: Channel -> Channel -> Process -> Process -> String
debugSubs y z q x = "substituting {" ++ show z ++ "/" ++ show y ++ "}" ++ " in " ++ show q ++ " --> " ++ show x
