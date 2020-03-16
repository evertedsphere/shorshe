{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Syntax where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Vector.Instances ()
import GHC.Exts (IsList)
import GHC.Generics (Generic)

type Vec =
  Vector

-- | A line/column position for error reporting.
-- TODO(mrkgnao): move to a byte offset-based representation
data SrcPos
  = SrcPos (Int, Int)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A source location
data SrcLoc
  = SrcLoc Text SrcPos SrcPos
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

invalid :: SrcPos
invalid =
  SrcPos (-1, -1)

-- | The source location for an inferred variable.
inferred :: SrcLoc
inferred =
  SrcLoc "inferred" invalid invalid

-- | A dummy source location.
dummy :: SrcLoc
dummy =
  SrcLoc "dummy" invalid invalid

-- Vars

newtype Var
  = MkVar Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

type Vars =
  Vec Var

newtype TyVar
  = MkTyVar Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

type TyVars =
  Vec TyVar

newtype FnVar
  = MkFnVar Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

newtype ProvVar
  = MkProvVar Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

newtype StructVar
  = MkStructVar Text
  deriving stock (Show, Eq, Generic)

data Owned
  = Shared
  | Unique
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

data EnvVar
  = MkEnvVar Owned Text
  deriving stock (Show, Eq, Generic)

type EnvVars =
  Vec EnvVar

-- Paths and places

data SubtypeModality
  = Combine
  | Override
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

type Field =
  Text

type TypedField =
  (Field, Ty)

data PathEntry
  = Field Field
  | Index Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

newtype Path
  = Path (Vec PathEntry)
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable, IsList)

data PrePlace
  = PrePlace Var Path
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

data Place
  = Place SrcLoc PrePlace
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

type Places =
  Vec Place

-- Expession paths and places

data ExpPathEntry
  = ExpField Field
  | ExpIndex Int
  | ExpDeref
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

newtype ExpPath
  = ExpPath (Vec ExpPathEntry)
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable)

data PrePlaceExp
  = PrePlaceExp Var ExpPath
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

-- | A place expession.
data PlaceExp
  = PlaceExp SrcLoc PrePlaceExp
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

type PlaceExps =
  Vec PlaceExp

rootOf :: Place -> Var
rootOf (Place _ (PrePlace v _)) = v

pathOf :: Place -> Path
pathOf (Place _ (PrePlace _ p)) = p

expRootOf :: PlaceExp -> Var
expRootOf (PlaceExp _ (PrePlaceExp v _)) = v

expPathOf :: PlaceExp -> ExpPath
expPathOf (PlaceExp _ (PrePlaceExp _ p)) = p

toExpPathEntry :: PathEntry -> ExpPathEntry
toExpPathEntry = \case
  Field f -> ExpField f
  Index i -> ExpIndex i

toExpPath :: Path -> ExpPath
toExpPath (Path p) = ExpPath (toExpPathEntry <$> p)

toPlaceExp :: Place -> PlaceExp
toPlaceExp (Place loc (PrePlace root path)) =
  PlaceExp loc (PrePlaceExp root (toExpPath path))

-- | Convers expession path entries to the corresponding path entry forms.
toPathEntry :: ExpPathEntry -> Maybe PathEntry
toPathEntry = \case
  ExpField f -> Just (Field f)
  ExpIndex i -> Just (Index i)
  ExpDeref -> Nothing

toPath :: ExpPath -> Maybe Path
toPath (ExpPath ep) = foldr go (Just (Path [])) ep
  where
    go expEntry acc = do
      Path path <- acc
      entry <- toPathEntry expEntry
      pure (Path (Vec.cons entry path))

isPlace :: PlaceExp -> Bool
isPlace pexp = not (Vec.elem ExpDeref entrys)
  where
    ExpPath entrys = expPathOf pexp

data Loan
  = Loan {_loanOwned :: Owned, _loanPlaceExp :: PlaceExp}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

type Loans =
  Vec Loan

data Prov
  = Prov
      { _provSrcLoc :: SrcLoc,
        _provProvVar :: ProvVar
      }
  deriving stock (Show, Eq, Generic)

newtype Provs
  = Provs (Vec Prov)
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsList, Semigroup, Monoid)

containsProv :: Prov -> Provs -> Bool
containsProv (Prov _ v) (Provs provs) =
  provs & fmap _provProvVar & Vec.elem v

data Bound
  = Bound Prov Prov
  deriving stock (Show, Eq, Generic)

upperProvVar :: Bound -> ProvVar
upperProvVar (Bound _ (Prov _ v)) = v

newtype Bounds
  = Bounds (Vec Bound)
  deriving stock (Show)
  deriving newtype (IsList, Semigroup, Monoid)

instance Eq Bounds where
  Bounds bs == Bounds bs' = upperProvVars bs == upperProvVars bs'
    where
      upperProvVars = HashSet.fromList . Vec.toList . fmap upperProvVar

data Param
  = Param {_paramVar :: Var, _paramTy :: Ty}
  deriving stock (Show, Eq, Generic)

type Params =
  Vec Param

data BaseTy
  = Bool
  | U32
  | Unit
  deriving stock (Show, Eq, Generic)

data PreTy
  = TyAny
  | TyInfer
  | TyBase BaseTy
  | TyVar TyVar
  | TyRef Prov Owned Ty
  | TyFun EnvVars Provs TyVars Tys Env Ty Bounds
  | TyArr Ty Int
  | TySlice Ty
  | TyRec (Vec TypedField)
  | TyTup Tys
  | TyStruct StructVar Provs Tys (Maybe Ty)
  | TyUninit Ty
  deriving stock (Show, Eq, Generic)

data Ty
  = Ty SrcLoc PreTy
  deriving stock (Show, Eq, Generic)

data Env
  = EnvUnboxed
  | EnvVar EnvVar
  | Env VarEnv
  | EnvOf Var
  deriving stock (Show, Eq, Generic)

type Envs =
  Vec Env

newtype VarEnv
  = VarEnv (Vec Param)
  deriving stock (Show, Eq, Generic)

type Gamma =
  VarEnv

emptyGamma :: VarEnv
emptyGamma =
  VarEnv []

type Tys =
  Vec Ty

data PreTyCtx
  = CtxHole
  | CtxTy Ty
  | CtxTagged StructVar Provs Tys TyCtx
  | CtxRec (Vec (Field, TyCtx))
  | CtxTup (Vec TyCtx)
  deriving stock (Show, Eq, Generic)

-- | Invariant: a type-context should only ever have one hole.
data TyCtx
  = TyCtx SrcLoc PreTyCtx
  deriving stock (Show, Eq, Generic)

isSized :: Ty -> Bool
isSized (Ty _ ty) = case ty of
  TySlice _ -> False
  _ -> True

class Initck a where
  isInit :: a -> Bool

instance Initck Ty where
  isInit (Ty _ typ) = case typ of
    TyAny -> True
    TyInfer -> True
    TyBase {} -> True
    TyVar {} -> True
    -- invariant: this should always be true
    TyRef _ _ ty -> isInit ty
    TyFun _ _ _ tys gamma ty _ ->
      isInit tys && isInit gamma && isInit ty
    TyArr ty _ -> isInit ty
    TySlice ty -> isInit ty
    TyRec flds -> isInit (Vec.map snd flds)
    TyStruct _ _ tys (Just ty) -> isInit tys && isInit ty
    TyStruct _ _ tys Nothing -> isInit tys
    TyUninit {} -> False

instance Initck Tys where
  isInit = all isInit

instance Initck Env where
  isInit = \case
    EnvUnboxed -> True
    EnvVar {} -> True
    Env (VarEnv params) -> isInit (Vec.map _paramTy params)
    EnvOf {} -> True

data Prim
  = PrimTrue
  | PrimFalse
  | PrimUnit
  | PrimNum Int
  deriving stock (Show, Eq, Generic)

data Binop
  = -- | addition
    Add
  | -- | subtraction
    Sub
  | -- | multiplication
    Mul
  | -- | division
    Div
  | -- | remainder
    Rem
  | -- | boolean and
    And
  | -- | boolean or
    Or
  | -- | bitwise xor
    BitXor
  | -- | bitwise and
    BitAnd
  | -- | bitwise or
    BitOr
  | -- | shift left
    Shl
  | -- | shift right
    Shr
  | -- | equal
    Eq
  | -- | less than
    Lt
  | -- | less than or equal to
    Le
  | -- | not equal
    Ne
  | -- | greater than or equal to
    Ge
  | -- | greater than
    Gt
  deriving stock (Show, Eq, Generic)

data PreExp
  = ExpPrim Prim
  | ExpBinop Binop Exp Exp
  | ExpMove PlaceExp
  | ExpDrop PlaceExp
  | ExpBorrow Prov Owned PlaceExp
  | ExpBorrowSlice Prov Owned PlaceExp Exp Exp
  | ExpLetProv Provs Exp
  | ExpLet Var Ty Exp Exp
  | ExpAssign PlaceExp Exp
  | ExpSeq Exp Exp
  | ExpFn FnVar
  | ExpFun Provs TyVars Params (Maybe Ty) Exp
  | ExpApp Exp (Vec Env) Provs Tys
  | ExpIdx PlaceExp Exp
  | ExpAbort Text
  | ExpBranch Exp Exp Exp
  | ExpWhile Exp Exp
  | ExpFor Var Exp Exp
  | ExpTup Exps
  | ExpArr Exps
  | ExpRecStruct StructVar Provs Tys (Vec (Field, Exp))
  | ExpTupStruct StructVar Provs Tys Exps
  | ExpPtr Owned Place
  deriving stock (Show, Eq, Generic)

data Exp
  = Exp SrcLoc PreExp
  deriving stock (Show, Eq, Generic)

type Exps =
  Vec Exp

data Value
  = ValDead
  | ValPrim Prim
  | ValFun Provs TyVars Params Exp
  | ValTup (Vec Value)
  | ValArr (Vec Value)
  | ValPtr Owned Place
  deriving stock (Show, Eq, Generic)

newtype Store
  = Store (Vec (Var, Value))
  deriving stock (Show, Eq, Generic)

data FnDef
  = FnDef
      { _fnDefName :: FnVar,
        _fnDefEnvVars :: EnvVars,
        _fnDefProvs :: Provs,
        _fnDefTyVars :: TyVars,
        _fnDefParams :: Params,
        -- | Return type
        _fnDefReturnType :: Ty,
        _fnDefBounds :: Bounds,
        _fnDefBody :: Exp
      }
  deriving stock (Show, Eq, Generic)

makeFields ''FnDef

-- TODO(mrkgnao): convert to record, what is the bool?
data RecStructDef
  = RecStructDef Bool StructVar Provs TyVars (Vec TypedField)
  deriving stock (Show, Eq, Generic)

-- TODO(mrkgnao): convert to record, what is the bool?
data TupStructDef
  = TupStructDef Bool StructVar Provs TyVars Tys
  deriving stock (Show, Eq, Generic)

data StructDef
  = StructRec RecStructDef
  | StructTup TupStructDef
  deriving stock (Show, Eq, Generic)

data GlobalDef
  = DefFn FnDef
  | DefRecStruct RecStructDef
  | DefTupStruct TupStructDef
  deriving stock (Show, Eq, Generic)

newtype GlobalEnv
  = GlobalEnv (Vec GlobalDef)
  deriving stock (Show, Eq, Generic)

type Sigma =
  GlobalEnv

emptySigma :: GlobalEnv
emptySigma =
  GlobalEnv []

findFnDef :: GlobalEnv -> FnVar -> Maybe FnDef
findFnDef (GlobalEnv sigma) fn = do
  DefFn fnDef <- Vec.find isRightFn sigma
  pure fnDef
  where
    isRightFn :: GlobalDef -> Bool
    isRightFn = \case
      DefFn fnDef -> fn == _fnDefName fnDef
      DefTupStruct (TupStructDef _ (MkStructVar v) _ _ _) -> fn == MkFnVar v
      DefRecStruct {} -> False

data SubtypeRel
  = SubtypeRel ProvVar ProvVar
  deriving stock (Show, Eq, Generic)

data TyVarEnv
  = TyVarEnv
      { _tyVarEnvEnvVars :: EnvVars,
        _tyVarEnvProvs :: Provs,
        _tyVarEnvTyVars :: TyVars,
        _tyVarEnvBounds :: Vec SubtypeRel
      }
  deriving stock (Show, Eq, Generic)

makeFields ''TyVarEnv

addEnvVars :: HasEnvVars a EnvVars => EnvVars -> a -> a
addEnvVars evs = envVars <>~ evs

addProvs :: HasProvs a Provs => Provs -> a -> a
addProvs ps = provs <>~ ps

addTyVars :: HasTyVars a TyVars => TyVars -> a -> a
addTyVars tvs = tyVars <>~ tvs

addBounds :: HasBounds a Bounds => Bounds -> a -> a
addBounds bds = bounds <>~ bds

containsProv' :: HasProvs a Provs => Prov -> a -> Bool
containsProv' p a = containsProv p (view provs a)

containsTyVar :: HasTyVars a TyVars => TyVar -> a -> Bool
containsTyVar = elemOf (tyVars . folded)

containsEnvVar :: HasEnvVars a EnvVars => EnvVar -> a -> Bool
containsEnvVar = elemOf (envVars . folded)

-- | TODO(mrkgnao): what is this? better name
absSub :: HasBounds a (Vec SubtypeRel) => Prov -> Prov -> a -> Bool
absSub (Prov _ v1) (Prov _ v2) a =
  SubtypeRel v1 v2 `Vec.elem` view bounds a || v1 == v2

type Delta =
  TyVarEnv

emptyDelta :: TyVarEnv
emptyDelta =
  TyVarEnv [] [] [] []

newtype LoanEnv
  = LoanEnv (Vec (Prov, Loans))
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

type Ell =
  LoanEnv

emptyEll :: LoanEnv
emptyEll =
  LoanEnv []

placesOf :: LoanEnv -> Vec PlaceExp
placesOf (LoanEnv ell) = do
  (_, loans) <- ell
  _loanPlaceExp <$> loans

domainOf :: LoanEnv -> Provs
domainOf (LoanEnv ell) = Provs do
  (prov, _) <- ell
  pure prov

-- TODO are these just kv pair list maps then? use real maps?
toProvVarMap :: LoanEnv -> HashMap ProvVar Loans
toProvVarMap (LoanEnv concrete) =
  HashMap.fromList
    $ Vec.toList
    $ concrete
      <&> \(Prov _ v, loans) -> (v, loans)

loanEnvContainsProv :: LoanEnv -> Prov -> Bool
loanEnvContainsProv ell (Prov _ var) = toProvVarMap ell & HashMap.member var

loanEnvLookup :: LoanEnv -> Prov -> Maybe Loans
loanEnvLookup ell (Prov _ var) = toProvVarMap ell & HashMap.lookup var

loanEnvLookup' :: LoanEnv -> Prov -> Loans
loanEnvLookup' ell prov = fromJust $ loanEnvLookup ell prov

unorderedEq :: (Eq a, Hashable a) => Vec a -> Vec a -> Bool
unorderedEq v v' = vectorToSet v == vectorToSet v'
  where
    vectorToSet = HashSet.fromList . Vec.toList

instance Eq LoanEnv where
  ell == ell' =
    unorderedEq (concreteDom ell) (concreteDom ell') && all equalLoans provs
    where
      -- FIXME why does this differ from aatxe's code?
      concreteDom :: LoanEnv -> Vec Owned
      concreteDom (LoanEnv e) = e >>= snd <&> _loanOwned
      equalLoans prov =
        unorderedEq (loanEnvLookup' ell prov) (loanEnvLookup' ell' prov)
      provs = let LoanEnv e = ell in Vec.map fst e

loanEnvFilterDom :: LoanEnv -> Provs -> LoanEnv
loanEnvFilterDom (LoanEnv ell) (Provs provs) =
  let provVars = fmap (\(Prov _ p) -> p) provs
   in LoanEnv (Vec.filter (\(Prov _ p, _) -> p `Vec.elem` provVars) ell)

loanEnvInclude :: Prov -> Loans -> LoanEnv -> LoanEnv
loanEnvInclude prov@(Prov _ var) loans (LoanEnv ell) =
  LoanEnv
    ( ell
        & Vec.filter (\(Prov _ var', _) -> var /= var')
        & Vec.cons (prov, loans)
    )

loanEnvIncludeAll :: Provs -> Loans -> LoanEnv -> LoanEnv
loanEnvIncludeAll (Provs provs) loans (LoanEnv ell) =
  LoanEnv
    ( Vec.filter
        ( \(prov, _) ->
            provs
              & Vec.elem prov
              & not
        )
        ell
        Vec.++ entries
    )
  where
    entries = provs <&> (\prov -> (prov, loans))

-- loanEnvExclude ::

-- | Useful for pretty-printing in ownership safety
newtype PlaceEnv
  = PlaceEnv (Vec (Place, Ty))
  deriving stock (Show, Eq, Generic)

data StructKind
  = Rec
  | Tup
  deriving stock (Show, Eq, Generic)

data TcErr
  = -- | expected, found
    TypeMismatch Ty Ty
  | TypeMismatchIterable Ty
  | TypeMismatchFunction Ty
  | TypeMismatchRef Ty
  | TypeMismatchArray Ty
  | VarEnvMismatch SrcLoc VarEnv VarEnv
  | LoanEnvMismatch SrcLoc LoanEnv LoanEnv
  | -- | attempted access, conflicting loan
    SafetyErr (Owned, PlaceExp) (Owned, PlaceExp)
  | PermissionErr Ty ExpPath Owned
  | NoReferenceToParameter Place
  | NoReferenceToCaptured Place
  | CannotMove PlaceExp
  | -- | exp in function position, the uninitialized type
    MovedFunction Exp Ty
  | -- | place that was moved the type for it
    PartiallyMoved Place Ty
  | -- | the type of the path that was moved
    PartiallyMovedPath Ty Path
  | -- | the type of the exp path that was moved
    PartiallyMovedExpPath Ty ExpPath
  | -- | uninitialized type initialized type
    PartiallyMovedTypes Ty Ty
  | -- | lhs is not a subType rhs
    UnificationFailed Ty Ty
  | UnknownFunction SrcLoc FnVar
  | UnknownStruct SrcLoc StructVar
  | UnevaluatedEnvOf Var
  | -- | the qualifier the environment used
    UnsatisfiedEnvQualifier Owned Env
  | WrongStructConstructor SrcLoc StructVar StructKind
  | -- | return Type invalidated Provenance
    InvalidReturnType Ty Prov
  | InvalidType Ty
  | -- | invalid env var the Type it was found in
    InvalidEnvVar EnvVar Ty
  | InvalidProv Prov
  | -- | the first provenance does not outlive the second
    ProvDoesNotOutlive Prov Prov
  | InvalidLoan Owned PlaceExp
  | InvalidArrayLen Ty Int
  | InvalidOperationOnType Path Ty
  | InvalidOperationOnTypeEP ExpPath Ty
  | DuplicateFieldsInStructDef StructVar TypedField TypedField
  | -- | for struct, because of Ty
    InvalidCopyImpl StructVar Ty
  | UnboundPlace Place
  | UnboundPlaceExp PlaceExp
  | -- | unbound loan in Prov
    UnboundLoanInProv PlaceExp Prov
  | PlaceExpNotAPlace PlaceExp
  | AbsProvsNotSubtype Prov Prov
  | -- | cannot promote local Prov to abstract Prov
    CannotPromoteLocalProvToAbstract Prov Prov
  | EnvArityMismatch Text Envs EnvVars
  | ProvArityMismatch Text Provs Provs
  | TysArityMismatch Text Tys Tys
  | TyArityMismatch Text Tys TyVars
  | ExpArityMismatch Text Exps Tys
  deriving stock (Show, Eq, Generic)

newtype TcT m a = TcT {unTcT :: ExceptT TcErr m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError TcErr
    )

type Tc = TcT Identity

combineEnvs :: Monad m => Text -> Envs -> EnvVars -> TcT m (Vec (Env, EnvVar))
combineEnvs ctx e e' =
  if length e /= length e'
    then throwError (EnvArityMismatch ctx e e')
    else pure (Vec.zip e e')
