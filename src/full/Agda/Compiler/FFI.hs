{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}

module Agda.Compiler.FFI where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Reader

import Agda.Syntax.Common
import Agda.Syntax.Internal
import Agda.Syntax.Treeless (TTerm (..))
import Agda.TypeChecking.Monad hiding (Polarity (..), Context)
import Agda.TypeChecking.Monad.Builtin
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Reduce
import Agda.TypeChecking.Free
import Agda.Compiler.Treeless.Subst

import Agda.Utils.Impossible

#include "undefined.h"

levelKit :: TCM QName
levelKit = do
  Def lvl _ <- primLevel
  return lvl

dropSetLevelArgs :: Type -> TTerm -> TCM TTerm
dropSetLevelArgs ty t = do
  lvlKit <- levelKit
  dropSetLevelArgs' lvlKit ty t

data CContext = CCovariant | CContravariant
  deriving (Eq, Show)

invertCContext :: CContext -> CContext
invertCContext CCovariant = CContravariant
invertCContext CContravariant = CCovariant

-- | Creates a wrapper for FFI calls, discarding level and set arguments.
dropSetLevelArgs' :: QName -> Type -> TTerm -> TCM TTerm
dropSetLevelArgs' lvlNm t body = wrapType CCovariant body t
  where
    wrapArgs c body = foldM (wrapTerm c) body . map unArg
    wrapType c body = wrapTerm c body . unEl
    wrapTerm :: CContext -> TTerm -> Term -> TCM TTerm
    wrapTerm c body v = do
      v   <- unSpine <$> reduce v
      reportSLn "compile.haskell.type" 50 $ "toHaskellType " ++ show v
      kit <- liftTCM coinductionKit
      case ignoreSharing v of
        Var x es -> return body
        Def d _ | d == lvlNm && c == CCovariant-> return (TApp body [TUnit])
                | otherwise -> return body
        Sort{}  | c == CCovariant -> return (TApp body [TSort])
                | otherwise -> __IMPOSSIBLE__
        Pi a b | c == CContravariant -> do
          (lam, body') <- case unEl (unDom a) of
            Def nm _ | nm == lvlNm -> return (id, TApp body [TUnit])
            Sort{} -> return (id, TApp body [TSort])
            _ -> do
              a' <- wrapType (invertCContext c) (TVar 0) (unDom a)
              return (TLam, TApp (wk body) [a'])
          lam <$> checkPiRight c body' a b
        Pi a b | otherwise -> do
          body' <- case unEl (unDom a) of
            Def nm _ | nm == lvlNm -> return $ wk body
            Sort{} -> return $ wk body
            _ -> do
              a' <- wrapType (invertCContext c) (TVar 0) (unDom a)
              return $ TApp (wk body) [a']
          TLam <$> checkPiRight c body' a b
        Con c args -> return body
        Lit{}      -> return body
        Level{}    -> __IMPOSSIBLE__
        Shared{}   -> __IMPOSSIBLE__
        MetaV{}    -> __IMPOSSIBLE__
        DontCare{} -> __IMPOSSIBLE__
        Lam{}      -> __IMPOSSIBLE__
    wk = applySubst (wkS 1 idS)
    checkPiRight c body a b = do
      if isBinderUsed b  -- Andreas, 2012-04-03.  Q: could we rely on Abs/NoAbs instead of again checking freeness of variable?
        then do
          underAbstraction a b $ \b ->
            wrapType c body b
        else wrapType c body (noabsApp __IMPOSSIBLE__ b)

