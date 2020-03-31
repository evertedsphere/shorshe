
module Shorshe.Syntax.Literal where

import Data.Word

import Shorshe.Syntax.Abstract.Name
import Shorshe.Syntax.Position


data Literal
  = LitNat    Interval !Word32
  | LitWord64 Interval !Word64
  | LitFloat  Interval !Double
  | LitString Interval String
  | LitChar   Interval !Char
  | LitQName  Interval QName
