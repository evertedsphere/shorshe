{-# LANGUAGE OverloadedLists #-}

module Meta where

import Control.Lens
import Control.Monad.Except
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import qualified Data.Vector as Vec
import Syntax

type Vec = Vec.Vector

-- | TODO isn't this just an Ord function
isAtLeast :: Owned -> Owned -> Bool
isAtLeast o o' = case (o, o') of
  (Shared, _) -> True
  (Unique, Unique) -> True
  (Unique, Shared) -> False

decomposeBy :: Path -> Ty -> Tc (TyCtx, Ty)
decomposeBy path (Ty loc ty) = case (ty, path) of
  (ty, []) -> pure (TyCtx inferred CtxHole, Ty loc ty)
  (TyRec pairs, Path (Field f :< path)) -> do
    (inner_ctx, inner_ty) <-
      pairs
        & Vec.toList
        & HashMap.fromList
        & HashMap.lookup f
        & fromJust
        & decomposeBy (Path path)
    let replace :: (Field, Ty) -> (Field, TyCtx)
        replace pair@(fld, ty@(Ty loc _)) =
          ( fld,
            if fld == f
              then inner_ctx
              else TyCtx loc (CtxTy ty)
          )
    let ctx = TyCtx loc (CtxRec $ fmap replace pairs)
    pure (ctx, inner_ty)
  -- TODO finish rest of cases
  (ty, path) ->
    throwError $
      InvalidOperationOnType path (Ty loc ty)
