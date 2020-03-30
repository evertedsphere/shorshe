{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedLists            #-}

module Shorshe.Syntax.Position where

import           Control.Exception
import           Data.Function
import           Data.Sequence as Seq
import           Data.Word

data PositionExcept
  = PErrFileMismatch

instance Show PositionExcept where
  show PErrFileMismatch = "internal error: positions comparison with heterogeneous first projections.\n"

instance Exception PositionExcept where

class HasFile f where
  dropFile :: f a -> f ()
  fileFrom :: f a -> a

  invar :: Eq a => f a -> f a -> Bool
  invar = (==) `on` fileFrom

-- An offset in the file `offPath`
data FileOffset a = FileOffset
  { offPath :: a       -- File name
  , offPos  :: !Word32 -- File offset
  } deriving Eq

instance Eq a => Ord (FileOffset a) where
  FileOffset fp1 x <= FileOffset fp2 y
    | fp1 == fp2 = x <= y
    | otherwise  = throw PErrFileMismatch

instance Functor FileOffset where
  fmap f (FileOffset x off) = FileOffset (f x) off

instance HasFile FileOffset where
  dropFile (FileOffset _ pos) = FileOffset () pos
  fileFrom (FileOffset x _)   = x


--------------------------------------------------------------------------------
--

type Position    = Position' ()
data Position' a = Position
  { posOff  :: !(FileOffset a)
  , posLine :: !Word32 -- File line (counting from 0)
  , posCol  :: !Word32 -- File column
  }

instance Eq a => Eq (Position' a) where
  (==) = (==) `on` posOff

instance Eq a =>  Ord (Position' a) where
  (<=) = (<=) `on` posOff

instance Functor Position' where
  fmap f (Position x line col) = Position (fmap f x) line col

instance HasFile Position' where
  dropFile  (Position x line col) = Position (dropFile x) line col

  fileFrom  (Position x _ _)   = fileFrom x


--------------------------------------------------------------------------------
--

type Interval    = Interval' ()
data Interval' a = Interval
  { intFile  :: a
  , intStart :: !Position
  , intEnd   :: !Position
  } deriving Eq

mkInterval :: Eq a => Position' a -> Position' a -> Interval' a
mkInterval p1 p2
  | p1 `invar` p2 = (Interval (fileFrom p1) `on` dropFile) start end
  | otherwise     = throw PErrFileMismatch
  where start = min p1 p2
        end   = max p1 p2

intersects :: Eq a => Interval' a -> Interval' a -> Bool
intersects i1 i2
  | i1 `invar` i2 = intEnd i1 >= intStart i2
  | otherwise     = throw PErrFileMismatch

disjoint :: Eq a => Interval' a -> Interval' a -> Bool
disjoint i1 i2
  | i1 `invar` i2 = intEnd i1 < intStart i2
  | otherwise     = throw PErrFileMismatch

instance HasFile Interval' where
  dropFile (Interval _ st en) = Interval () st en

  fileFrom (Interval x _  _)  = x

instance Eq a => Ord (Interval' a) where
  i1 <= i2 = intersects i1 i2 || intEnd i1 <= intStart i2


--------------------------------------------------------------------------------
--

data Normalized
  = Normal
  | Abnormal

newtype Intervals (n :: Normalized) a = Intervals (a, Seq Interval)
  deriving Functor

singleInts :: Interval' a -> Intervals 'Normal a
singleInts int = Intervals (fileFrom int, [ dropFile int ])

fuse :: Interval -> Interval -> Seq Interval
fuse x y = if intersects x y
  then singleton $ x { intEnd = intEnd y }
  else [ x , y ]

fusion :: Seq Interval -> Seq Interval
fusion Empty            = Empty
fusion (x :<| Empty)    = x <| Empty
fusion (x :<| y :<| ss) = fuse x y >< ss

normalise :: Intervals 'Abnormal a -> Intervals 'Normal a
normalise (Intervals (fp, is)) = Intervals (fp, fusion is)

instance Eq a => Semigroup (Intervals 'Normal a) where
  Intervals (x, ints1) <> Intervals (y, ints2)
    | x == y = Intervals (x , case (ints1, ints2) of
      (Empty     , is)         -> is
      (is        , Empty)      -> is
      (is1 :|> i1, i2 :<| is2) -> case compare i1 i2 of
        LT -> (is1 |> i1) >< (i2 <| is2)
        EQ -> is1 >< i1 { intEnd = intEnd i2 } <| is2
        GT -> let (leftInter,  is2') = breakl (i1 <) is2
                  (rightInter, is1') = breakr (i2 >) is1
              in is1' >< sort (fusion $ leftInter >< rightInter) >< is2')
    | otherwise = throw PErrFileMismatch

instance HasFile (Intervals n) where
  dropFile (Intervals (_, is)) = Intervals ((), is)
  fileFrom (Intervals (x, _))  = x


--------------------------------------------------------------------------------
--

-- TODO(rizoid): `String` a proper source file
type Span = Intervals 'Normal String

class HasSpan a where
  getSpan :: a -> Maybe Span
