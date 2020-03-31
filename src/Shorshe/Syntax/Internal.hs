
module Shoreshe.Syntax.Internal where

data Elim' a
  = Apply (Arg a)
  | Proj  ProjOrigin QName
  | IApply


--------------------------------------------------------------------------------
--

data Term
  = Var {-# UNPACK #-} !Int Elims
