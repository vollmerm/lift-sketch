{-# LANGUAGE GADTs #-}


module Data.Skel.AST ( AExp(...), Exp(...)  )
       where

import Data.List

type Arr = List

data Exp a where
  Scalar      :: (Num n) => n -> Exp n
  LitBool     :: Bool -> Exp Bool
  Plus        :: (Num n) => Exp n -> Exp n -> Exp n
  Minus       :: (Num n) => Exp n -> Exp n -> Exp n
  Mult        :: (Num n) => Exp n -> Exp n -> Exp n
  If          :: Exp Bool -> Exp a -> Exp a -> Exp a
  Equal       :: (Num n) => Exp n -> Exp n -> Exp Bool
  LThan       :: (Num n) => Exp n -> Exp n -> Exp Bool
  GThan       :: (Num n) => Exp n -> Exp n -> Exp Bool

data AExp a where
  
