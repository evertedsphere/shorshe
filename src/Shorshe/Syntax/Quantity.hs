
module Shorshe.Syntax.Quantity where

import Data.Function

import Shorshe.Syntax.Position

data Q0Origin
  = Q0Inferred
  | Q0       Span
  | Q0Erased Span

instance HasSpan Q0Origin where
  getSpan Q0Inferred      = Nothing
  getSpan (Q0       span') = Just span'
  getSpan (Q0Erased span') = Just span'

instance Semigroup Q0Origin where
  o1 <> o2 = maybe Q0Inferred Q0 $ ((<>) `on` getSpan) o1 o2

instance Monoid Q0Origin where
  mempty = Q0Inferred


--------------------------------------------------------------------------------
--

data Q1Origin
  = Q1Inferred
  | Q1       Span
  | Q1Linear Span

instance HasSpan Q1Origin where
  getSpan Q1Inferred      = Nothing
  getSpan (Q1       span') = Just span'
  getSpan (Q1Linear span') = Just span'

instance Semigroup Q1Origin where
  o1 <> o2 = maybe Q1Inferred Q1 $ ((<>) `on` getSpan) o1 o2

instance Monoid Q1Origin where
  mempty = Q1Inferred


--------------------------------------------------------------------------------
--

data QωOrigin
  = QωInferred
  | Qω       Span
  | QωPlenty Span

instance HasSpan QωOrigin where
  getSpan QωInferred      = Nothing
  getSpan (Qω       span') = Just span'
  getSpan (QωPlenty span') = Just span'

instance Semigroup QωOrigin where
  o1 <> o2 = maybe QωInferred Qω $ ((<>) `on` getSpan) o1 o2

instance Monoid QωOrigin where
  mempty = QωInferred


--------------------------------------------------------------------------------
--

data Quantity
  = Quantity0 Q0Origin
  | Quantity1 Q1Origin
  | Quantityω QωOrigin

-- additive monoid (Q0 identity) over quantities
(+:+) :: Quantity -> Quantity -> Quantity
q1           +:+ Quantity0 _  = q1
Quantity0 _  +:+ q2           = q2
Quantity1 _  +:+ Quantity1 _  = Quantityω mempty
Quantityω q1 +:+ _            = Quantityω q1
_            +:+ Quantityω q2 = Quantityω q2

-- multiplicative monoid (Q1 identity) over quantities
(*:*) :: Quantity -> Quantity -> Quantity
Quantity1 _  *:* q2           = q2
q1           *:* Quantity1 _  = q1
_            *:* Quantity0 q2 = Quantity0 q2
Quantity0 q2 *:* _            = Quantity0 q2
_            *:* omega        = omega
