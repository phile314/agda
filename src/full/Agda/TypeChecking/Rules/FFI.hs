{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}

module Agda.TypeChecking.Rules.FFI where

import qualified Data.Map as Map
import Data.Typeable
import Data.Maybe
import Control.Monad

import Agda.Syntax.Internal
import Agda.Syntax.Common
import Agda.Syntax.Treeless

import Agda.TypeChecking.Monad hiding (FFIFunImport' (..), FFIFunImportSpec)
import qualified Agda.TypeChecking.Monad as TCM
import Agda.TypeChecking.Monad.Builtin
import Agda.TypeChecking.Unquote
import Agda.TypeChecking.Monad.Exception
import Agda.TypeChecking.Free
import Agda.TypeChecking.Reduce
import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Pretty
import qualified Agda.Compiler.JS.Parser as JS
import qualified Agda.Compiler.UHC.Pragmas.Parse as UHC

import Agda.Utils.Except
import Agda.Utils.Impossible

#include "undefined.h"

checkFFIFunImportSpec ::
    QName
  -> Type
  -> FFIFunImportSpec
  -> TCM ()
checkFFIFunImportSpec q ty imp = do
  mapM_ (checkFFIFunImport q ty) (Map.elems imp)

checkFFIFunImport ::
     QName -- ^ name of the axiom associated with this fun import
  -> Type
  -> FFIFunImport
  -> TCM ()
checkFFIFunImport q ty imp = do
  case imp of
    FunImp_JS_JS js -> do
--      checkFFIType Way_JS_JS ty
      case JS.parse js of
            Left js -> addFFIFunImport q Target_JS_JS (TCM.FunImp_JS_JS js)
            Right s ->
              typeError (CompilationError ("Failed to parse ECMAScript (..." ++ s ++ ") for " ++ show q))

    FunImp_UHC_Core cr -> do
--      checkFFIType Way_UHC_Core ty
      case UHC.parseCoreExpr cr of
        Left msg -> typeError $ GenericError $ "Could not parse UHC foreign import: " ++ msg
        Right cr -> addFFIFunImport q Target_UHC_Native (TCM.FunImp_UHC_Core cr)

    FunImp_UHC_HS hsNm -> do
--      checkFFIType Way_UHC_HS ty
      case UHC.parseCoreExpr hsNm of
        Left msg -> typeError $ GenericError $ "Could not parse UHC foreign import: " ++ msg
        Right cr -> addFFIFunImport q Target_UHC_Native (TCM.FunImp_UHC_HS cr)

    FunImp_MAZ_HS hs -> do
--      checkFFIType Way_MAZ_HS ty
      addFFIFunImport q Target_MAZ_Native (TCM.FunImp_MAZ_HS hs)




notAFFIType :: FFIWay -> Type -> FFI ()
notAFFIType w t = do
  -- TODO don't use show!
  err <- fsep $ pwords "The type" ++ [prettyTCM t] ++
                       pwords "cannot be translated to a " -- TODO ++ {- TODO -[prettyShow w] ++-} " type."
  typeError $ GenericError $ show err


data CContext = CCovariant | CContravariant
  deriving (Eq)

invertCContext :: CContext -> CContext
invertCContext CCovariant = CContravariant
invertCContext CContravariant = CCovariant

type FFI = TCM --ReaderT Context TCM

ensureContravariant :: CContext -> String -> FFI ()
ensureContravariant c err = do
--  t <- ask
  when (c /= CContravariant) (typeError $ GenericError err)

levelKit :: TCM QName
levelKit = do
  Def lvl _ <- primLevel
  return lvl

checkFFIType :: FFIWay -> Type -> TCM ()
checkFFIType w t = do
  lvlKit <- levelKit
  checkFFIType' lvlKit w t

checkFFIType' :: QName -> FFIWay -> Type -> TCM ()
checkFFIType' lvl w t = do
  checkType CCovariant t
  where
    err      = notAFFIType w t
    checkArgs c = mapM_ (checkTerm c . unArg)
    checkType c = checkTerm c . unEl
    checkTerm :: CContext -> Term -> FFI ()
    checkTerm c v = do
      v   <- unSpine <$> reduce v
      reportSLn "tc.ffi.check" 50 $ "checkTerm " ++ show v
      kit <- liftTCM coinductionKit
      case v of
        Var x es -> do
          let args = fromMaybe __IMPOSSIBLE__ $ allApplyElims es
          checkArgs c args
        Def d es | Just d == (nameOfInf <$> kit) ->
          case es of
            [Apply a, Apply b] -> checkTerm c (unArg b)
            _                  -> err
        Def d es | d == lvl ->
          ensureContravariant c "Level arguments can only be used in contravariant position in FFI calls."
        Def d es -> do
          let args = fromMaybe __IMPOSSIBLE__ $ allApplyElims es
          d' <- defCompiledRep <$> getConstInfo d
          case () of
            _ | Just _ <- getFFITypeBind w d' -> checkArgs c args
            _ | otherwise -> notAFFIType w (El Prop $ Def d [])
        Pi a b -> do
          checkType (invertCContext c) (unDom a)
          if isBinderUsed b  -- Andreas, 2012-04-03.  Q: could we rely on Abs/NoAbs instead of again checking freeness of variable?
            then do
              underAbstraction a b $ \b ->
                checkType c b
            else checkType c (noabsApp __IMPOSSIBLE__ b)
        Con c args -> err -- TODO hsApp <$> getHsType (conName c) <*> fromArgs args
        Lam{}      -> err
        Level{}    -> err
        Lit{}      -> return ()
        Sort{}     -> ensureContravariant c "Set arguments can only be used in contravariant position in FFI calls."
        Shared p   -> checkTerm c $ derefPtr p
        MetaV{}    -> err
        DontCare{} -> err




-- types for unquoted FFI specs


type FFIFunImportSpec = Map.Map CompileTarget FFIFunImport

data FFIFunImport
  = FunImp_MAZ_HS   String {- Haskell code -}
  | FunImp_JS_JS    String {- JS code -}
  | FunImp_UHC_HS   String {- identifier -}
  | FunImp_UHC_Core String {- Core expr -}
  deriving (Typeable, Show)


instance Unquote (Map.Map CompileTarget FFIFunImport) where
  unquote t = do
    t <- reduceQuotedTerm t
    case ignoreSharing t of
      Con c xs | length xs == 3 ->
        choice
          [ (c `isRecCon` primFFIFunImportSpec, Map.fromList . catMaybes <$> traverse unquoteN xs)
          ]
          __IMPOSSIBLE__
      Con c _ -> __IMPOSSIBLE__
      _ -> throwException $ NotAConstructor "FFIFunImportSpec" t

instance Unquote (Maybe (CompileTarget, FFIFunImport)) where
  unquote t = do
    t <- reduceQuotedTerm t
    case ignoreSharing t of
      Con c [] -> do
        choice
          [ (c `isCon` primFFIFunImport_RuntimeError, return Nothing)
          ]
          __IMPOSSIBLE__
      Con c [x] -> do
        choice
          [ (c `isCon` primFFIFunImport_MAZ_HS,
            (\x -> Just (Target_MAZ_Native, FunImp_MAZ_HS x)) <$> unquoteNString x)
          , (c `isCon` primFFIFunImport_JS_JS,
            (\x -> Just (Target_JS_JS, FunImp_JS_JS x)) <$> unquoteNString x)
          , (c `isCon` primFFIFunImport_UHC_HS,
            (\x -> Just (Target_UHC_Native, FunImp_UHC_HS x)) <$> unquoteNString x)
          , (c `isCon` primFFIFunImport_UHC_Core,
            (\x -> Just (Target_UHC_Native, FunImp_UHC_Core x)) <$> unquoteNString x)
          ]
          __IMPOSSIBLE__
      Con c _ -> __IMPOSSIBLE__
      _ -> throwException $ NotAConstructor "FFIFunImport" t

