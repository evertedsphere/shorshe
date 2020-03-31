
module Shorshe.Syntax.Reflected where

import Data.Word

import Shorshe.Syntax.Abstract.Name
import Shorshe.Syntax.Common
import Shorshe.Syntax.Literal


--------------------------------------------------------------------------------
--

type    Elim    = Elim' Term
newtype Elim' a = Apply (Arg a)
  deriving Show


--------------------------------------------------------------------------------
--

newtype Abs a = Abs (String, a)


--------------------------------------------------------------------------------
-- types or terms, whatever you prefer to call them.

data Term
  = Var  Word32 [ Elim ]
  | Con  QName  [ Elim ]
  | Def  QName  [ Elim ]
  | Lam  (Abs Term)
  | Pi   (Arg Type) (Abs Type)
  | Sort Sort
  | Lit  Literal

type Type = Term


--------------------------------------------------------------------------------
-- sorts

data Sort
  = SetS Term
  | LitS Word32
  | UnknownS
