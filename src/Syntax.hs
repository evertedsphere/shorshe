{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vec

type Vec = Vector

-- | A line/column position for error reporting.
data Pos
  = Pos (Int, Int)
  deriving (Show)

-- | A source location
data SrcLoc
  = SrcLoc Text Pos Pos
  deriving (Show)

invalid :: Pos
invalid =
  Pos (-1, -1)

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
  = MkVar (Text)
  deriving (Show)

type Vars =
  Vec Var

newtype TyVar
  = MkTyVar (Text)
  deriving (Show)

type TyVars = Vec TyVar

newtype FnVar
  = MkFnVar (Text)
  deriving (Show)

newtype ProvVar
  = MkProvVar (Text)
  deriving (Show)

newtype StructVar
  = MkStructVar (Text)
  deriving (Show)

data Owned
  = Shared
  | Unique
  deriving (Show)

data EnvVar
  = MkEnvVar Owned Text
  deriving (Show)

type EnvVars =
  Vec EnvVar

-- Paths and places

data SubtypeModality
  = Combine
  | Override
  deriving (Show)

type Field =
  Text

type TypedField = (Field, Ty)

data PathEntry
  = Field Field
  | Index Int
  deriving (Show)

newtype Path
  = Path (Vec PathEntry)
  deriving (Show)

data PrePlace
  = PrePlace Var Path
  deriving (Show)

data Place
  = Place SrcLoc PrePlace
  deriving (Show)

type Places =
  Vec Place

-- Expession paths and places

data ExpPathEntry
  = ExpField Field
  | ExpIndex Int
  | ExpDeref
  deriving (Show)

newtype ExpPath
  = ExpPath (Vec ExpPathEntry)
  deriving (Show)

data PrePlaceExp
  = PrePlaceExp Var ExpPath
  deriving (Show)

data PlaceExp
  = PlaceExp SrcLoc PrePlaceExp
  deriving (Show)

type PlaceExps =
  Vec PlaceExp

rootOf :: Place -> Var
rootOf (Place _ (PrePlace v _)) = v

pathOf :: Place -> Path
pathOf (Place _ (PrePlace _ p)) = p

exprRootOf :: PlaceExp -> Var
exprRootOf (PlaceExp _ (PrePlaceExp v _)) = v

exprPathOf :: PlaceExp -> ExpPath
exprPathOf (PlaceExp _ (PrePlaceExp _ p)) = p

data Loan
  = Loan Owned PlaceExp
  deriving (Show)

type Loans =
  Vec Loan

data Prov
  = Prov SrcLoc ProvVar
  deriving (Show)

type Provs =
  Vec Prov

data Bound
  = Bound Prov Prov
  deriving (Show)

type Bounds =
  Vec Bound

data Param = Param Var Ty deriving (Show)

type Params = Vec Param

data BaseTy
  = Bool
  | U32
  | Unit
  deriving (Show)

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
  deriving (Show)

data Ty
  = Ty SrcLoc PreTy
  deriving (Show)

data Env
  = EnvUnboxed
  | EnvVar EnvVar
  | Env VarEnv
  | EnvOf Var
  deriving (Show)

type Envs = Vec Env

newtype VarEnv
  = VarEnv (Vec Param)
  deriving (Show)

type Gamma = VarEnv

emptyGamma :: VarEnv
emptyGamma = VarEnv []

type Tys =
  Vec Ty

data PreTyCtx
  = CtxHole
  | CtxTy Ty
  | CtxTagged StructVar Provs Tys TyCtx
  | CtxRec (Vec (Field, TyCtx))
  | CtxTup (Vec TyCtx)
  deriving (Show)

data TyCtx
  = TyCtx SrcLoc PreTyCtx
  deriving (Show)

data Prim
  = PrimTrue
  | PrimFalse
  | PrimUnit
  | PrimNum Int
  deriving (Show)

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
  deriving (Show)

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
  deriving (Show)

data Exp
  = Exp SrcLoc PreExp
  deriving (Show)

type Exps = Vec Exp

data Value
  = ValDead
  | ValPrim Prim
  | ValFun Provs TyVars Params Exp
  | ValTup (Vec Value)
  | ValArr (Vec Value)
  | ValPtr Owned Place
  deriving (Show)

newtype Store = Store (Vec (Var, Value)) deriving (Show)

data FnDef
  = FnDef
      { _fnDefName :: FnVar,
        _fnDefEnvVars :: EnvVars,
        _fnDefProvs :: Provs,
        _fnDefTyVars :: TyVars,
        _fnDefParams :: Params,
        -- | Return type
        _fnDefRetTy :: Ty,
        _fnDefBounds :: Bounds,
        _fnDefBody :: Exp
      }
  deriving (Show)

data RecStructDef
  = RecStructDef Bool StructVar Provs TyVars (Vec TypedField)
  deriving (Show)

data TupStructDef
  = TupStructDef Bool StructVar Provs TyVars Tys
  deriving (Show)

data StructDef
  = StructRec RecStructDef
  | StructTup TupStructDef
  deriving (Show)

data GlobalDef
  = DefFn FnDef
  | DefRec RecStructDef
  | DefTup TupStructDef
  deriving (Show)

newtype GlobalEnv = GlobalEnv (Vec GlobalDef) deriving (Show)

type Sigma = GlobalEnv

emptySigma :: GlobalEnv
emptySigma = GlobalEnv []

data SubTy = SubTy ProvVar ProvVar deriving (Show)

data TyVarEnv = TyVarEnv EnvVars Provs TyVars (Vec SubTy) deriving (Show)

type Delta = TyVarEnv

emptyDelta :: TyVarEnv
emptyDelta = TyVarEnv [] [] [] []

newtype LoanEnv = LoanEnv (Vec (Prov, Loans)) deriving (Show)

type Ell = LoanEnv

emptyEll :: LoanEnv
emptyEll = LoanEnv []

newtype PlaceEnv = PlaceEnv (Vec (Place, Ty)) deriving (Show)

data StructKind = Rec | Tup deriving (Show)

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
  | -- | expr in function position, the uninitialized type
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
  | InvalidOperationOnTypeEP
      ExpPath
      Ty
  | DuplicateFieldsInStructDef
      StructVar
      TypedField
      TypedField
  | -- | for struct, because of Ty
    InvalidCopyImpl StructVar Ty
  | UnboundPlace Place
  | UnboundPlaceExp PlaceExp
  | -- | unbound loan in Prov
    UnboundLoanInProv PlaceExp Prov
  | PlaceExpNotAPlace PlaceExp
  | AbsProvsNotSubType Prov Prov
  | -- | cannot promote local Prov to abstract Prov
    CannotPromoteLocalProvToAbstract Prov Prov
  | EnvArityMismatch Text Envs EnvVars
  | ProvArityMismatch Text Provs Provs
  | TysArityMismatch Text Tys Tys
  | TyArityMismatch Text Tys TyVars
  | ExpArityMismatch Text Exps Tys
  deriving (Show)
