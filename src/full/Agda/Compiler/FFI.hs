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

import Agda.Utils.Impossible

#include "undefined.h"




--checkFFIFunImport' :: FFIWay -> Type -> FFIFunImport



-- | Creates a wrapper for FFI calls, discarding level and set arguments.
dropSetLevelArgs :: Type -> TTerm -> TCM TTerm
dropSetLevelArgs t body = wrapType body t
  where
    wrapArgs body = foldM wrapTerm body . map unArg
    wrapType body = wrapTerm body . unEl
    wrapTerm :: TTerm -> Term -> TCM TTerm
    wrapTerm body v = do
      v   <- unSpine <$> reduce v
      reportSLn "compile.haskell.type" 50 $ "toHaskellType " ++ show v
      kit <- liftTCM coinductionKit
      case v of
        Var x es -> return body
        Def d es -> return body
        Pi a b -> do
          b' <- if isBinderUsed b  -- Andreas, 2012-04-03.  Q: could we rely on Abs/NoAbs instead of again checking freeness of variable?
            then do
              underAbstraction a b $ \b ->
                wrapType (TVar 0) b
            else wrapType (TVar 0) (noabsApp __IMPOSSIBLE__ b)
          case unEl (unDom a) of
            Level{} -> return $ TLam b'
            Sort{} -> return $ TLam b'
            _ -> do
              a' <- wrapType (TVar 0) (unDom a)
              return $ TLam (TLet a' $ TApp b' [TVar 0])
        Con c args -> return body
        Lit{}      -> return body
        Level{}    -> __IMPOSSIBLE__
        Sort{}     -> __IMPOSSIBLE__
        Shared p   -> wrapTerm body $ derefPtr p
        MetaV{}    -> __IMPOSSIBLE__
        DontCare{} -> __IMPOSSIBLE__
        Lam{}      -> __IMPOSSIBLE__


