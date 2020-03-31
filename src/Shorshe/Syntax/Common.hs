
module Shorshe.Syntax.Common where

import Data.Function
import Data.IntSet (IntSet)

import Shorshe.Syntax.Quantity


newtype Modality = Modality
  { modQuantity :: Quantity }
  deriving Show

instance Semigroup Modality where
  m1 <> m2 = Modality $ ((<>) `on` modQuantity) m1 m2

instance Monoid Modality where
  mempty = Modality mempty


--------------------------------------------------------------------------------
--

type FreeVariables = IntSet


--------------------------------------------------------------------------------
--

data ArgInfo = ArgInfo
  { argInfoModality :: Modality
  , argInfoFreeVars :: FreeVariables
  } deriving Show

data Arg a = Arg
  { argInfo :: ArgInfo
  , unArg   :: a
  } deriving Show
