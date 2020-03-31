
module Shorshe.Syntax.Abstract.Name where

import Data.Word

import Shorshe.Syntax.Position


data NameId = NameId {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving Show

data Name = Name
  { nameId          :: !NameId
  , nameBindingSite :: Interval
  }

newtype ModuleName = ModuleName
  { unModName :: [ Name ] }

data QName = QName
  { qNameModule :: ModuleName
  , qNameName   :: Name
  }
