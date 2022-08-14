{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Utils where

import Debug.Trace (trace)
import Syntax
import Language.Nominal

-- | Prints a string during execution of a function. For debugging.
debug :: c -> String -> c
debug = flip trace

-- Given a name ch and an abstraction abstr
-- it returns the body of abstr, using ch as the bound variable.
--getBody :: BinderConc ctxbody ctx body s a => a -> ctxbody -> body
getBody ch abstr = abstr `conc` ch

-- Given an abstraction, retrieves the label of the bound name, and returns a fresh name with this label.
getChannel :: Swappable t => KAbs (KName s t) a -> KName Tom t
getChannel abstr = id `genAppC` freshName (absLabel abstr)

-- Generates and returns a fresh channel with a given label.
freshChannel :: String -> Channel
freshChannel x = id `genAppC` freshName x

getDestState :: Transition -> State
getDestState (originSt, actDestSt, rule) = snd (getBody c actDestSt)
    where c = getChannel actDestSt

getOriginState :: Transition -> State
getOriginState (originSt,_,_) = originSt

getAction :: Transition -> Action
getAction (_,actDestSt,_) = fst (getBody c actDestSt)
    where c = getChannel actDestSt

-- Given a transition returns the list of applied rules
getRule :: Transition -> [Rule]
getRule (_, _, rule) = rule

-- Given a transition returns the abstracted channel to the (action, destState)
getAbstChannel :: Transition -> Channel
getAbstChannel (_,actDestSt,_) = getChannel actDestSt

-- Unpacks the elements of a transition into a tuple. 
-- Namely: (initial state, bound channel, action, destination state, applied rule)
unpack :: Transition -> (State, Channel, Action, State, [Rule])
unpack (originSt, actDestSt, rule) = (originSt, z, a, destSt, rule)
        where z = getChannel actDestSt
              (a, destSt) =  getBody z actDestSt


isTau_act :: Action -> Bool
isTau_act Tau_act = True
isTau_act _       = False

isOut_act :: Action -> Bool
isOut_act (Out_act _ _) = True
isOut_act _             = False

isIn_act :: Action -> Bool
isIn_act (In_act _ _) = True
isIn_act _            = False

-- Returns a list with the arguments of an action.
arguments_act :: Action -> [Channel]
arguments_act (Out_act x y) = [x, y]
arguments_act (In_act x y)  = [x, y]
arguments_act  Tau_act      = []

-- Returns all pair combinations between elements of two lists
combine :: [a] -> [a] -> [(a,a)]
combine l1 l2 = [ (x,y) | x<-l1, y<-l2 ]

-- mysub y z q returns q{z/y}
mysub :: Channel -> Channel -> Process -> Process
mysub y z (Out a b q)         = Out (sub y z a) (sub y z b) (mysub y z q)
mysub y z (Par p q)           = Par (mysub y z p) (mysub y z q)
mysub y z (In a abstraction)  = In (sub y z a) (mysub y z <$> abstraction)
mysub y z (New abstraction)   = New (mysub y z <$> abstraction)
mysub y z  Nil                = Nil
