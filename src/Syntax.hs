{-# LANGUAGE DeriveGeneric         
           , MultiParamTypeClasses 
           , DeriveAnyClass       -- to derive 'Swappable' 
           , DeriveDataTypeable   -- to derive 'Data' 
           , FlexibleInstances     
#-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


module Syntax where

import Language.Nominal
import Data.Generics hiding (Generic, typeof)
import GHC.Generics
import Prelude hiding ((||))

-- A Channel is an atom labelled with a String.
type Channel = Name String

data Process =    In   Channel (KAbs Channel Process)   -- c(x).P 
                | Out  Channel Channel Process          -- c<y>.P 
                | Par  Process Process                  -- P | Q  
                | New  (KAbs Channel Process)           -- (vx) P 
                | Nil                                   -- 0      
    deriving (Eq, Generic, Data, Swappable)

data Action = Tau_act
            | In_act   Channel Channel
            | Out_act  Channel Channel   
    deriving (Eq, Generic, Data, Swappable)

-- The states of the LTS are pi-calculus processes.
type State      = Process

-- An LTS transition is an origin state, a channel bound 
-- to the pair (Action, destination state) and the list of rules that have been applied.
type Transition = (State, KAbs Channel (Action, State), [Rule])

-- Applicable rules from LTS.
data Rule = Tau_r | Out_r | In_r | Par_r | Open_r | Res_r | Close_r | Com_r
    deriving(Eq, Generic, Data, Swappable, Show)

-- 'In' shortcut.
inp :: Channel -> Channel -> Process -> Process
inp c x p = In c (x @> p)

-- 'New' shortcut. 
rest :: Channel -> Process -> Process
rest x p = New (x @> p)

-- | Alternative syntax for 'Par'.
(||) :: Process -> Process -> Process
p || q = Par p q
infixl 9 ||



