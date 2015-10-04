{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Agda.TypeChecking.Rules.Builtin
  ( bindBuiltin
  , bindBuiltinNoDef
  , bindPostulatedName
  , isUntypedBuiltin
  , bindUntypedBuiltin
  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.List (find)

import qualified Agda.Syntax.Abstract as A
import qualified Agda.Syntax.Abstract.Views as A
import Agda.Syntax.Common
import Agda.Syntax.Internal
import Agda.Syntax.Position

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Monad.Builtin
import Agda.TypeChecking.Monad.SizedTypes ( builtinSizeHook )

import qualified Agda.TypeChecking.CompiledClause as CC
import Agda.TypeChecking.Conversion
import Agda.TypeChecking.Constraints
import Agda.TypeChecking.EtaContract
import Agda.TypeChecking.Irrelevance
import Agda.TypeChecking.Primitive
import Agda.TypeChecking.Reduce
import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Rules.Term ( checkExpr , inferExpr )

import {-# SOURCE #-} Agda.TypeChecking.Rules.Builtin.Coinduction
import {-# SOURCE #-} Agda.TypeChecking.Rewriting

import Agda.Utils.Except ( MonadError(catchError) )
import Agda.Utils.Maybe
import Agda.Utils.Monad
import Agda.Utils.Null
import Agda.Utils.Size

#include "undefined.h"
import Agda.Utils.Impossible

---------------------------------------------------------------------------
-- * Checking builtin pragmas
---------------------------------------------------------------------------

builtinPostulate :: TCM Type -> BuiltinDescriptor
builtinPostulate = BuiltinPostulate Relevant

coreBuiltins :: [BuiltinInfo]
coreBuiltins = map (\ (x, z) -> BuiltinInfo x z)
  [ (builtinList               |-> BuiltinData (tset --> tset) [builtinNil, builtinCons])
  , (builtinArg                |-> BuiltinData (tset --> tset) [builtinArgArg])
  , (builtinAbs                |-> BuiltinData (tset --> tset) [builtinAbsAbs])
  , (builtinArgInfo            |-> BuiltinData tset [builtinArgArgInfo])
  , (builtinBool               |-> BuiltinData tset [builtinTrue, builtinFalse])
  , (builtinNat                |-> BuiltinData tset [builtinZero, builtinSuc])
  , (builtinAgdaLiteral        |-> BuiltinData tset [builtinAgdaLitNat, builtinAgdaLitFloat,
                                                     builtinAgdaLitChar, builtinAgdaLitString,
                                                     builtinAgdaLitQName])
  , (builtinAgdaPattern        |-> BuiltinData tset [builtinAgdaPatVar, builtinAgdaPatCon, builtinAgdaPatDot,
                                                     builtinAgdaPatLit, builtinAgdaPatProj, builtinAgdaPatAbsurd])
  , (builtinAgdaPatVar         |-> BuiltinDataCons (tstring --> tpat))
  , (builtinAgdaPatCon         |-> BuiltinDataCons (tqname --> tlist (targ tpat) --> tpat))
  , (builtinAgdaPatDot         |-> BuiltinDataCons tpat)
  , (builtinAgdaPatLit         |-> BuiltinDataCons (tliteral --> tpat))
  , (builtinAgdaPatProj        |-> BuiltinDataCons (tqname --> tpat))
  , (builtinAgdaPatAbsurd      |-> BuiltinDataCons tpat)
  , (builtinLevel              |-> builtinPostulate tset)
  , (builtinInteger            |-> builtinPostulate tset)
  , (builtinFloat              |-> builtinPostulate tset)
  , (builtinChar               |-> builtinPostulate tset)
  , (builtinString             |-> builtinPostulate tset)
  , (builtinQName              |-> builtinPostulate tset)
  , (builtinIO                 |-> builtinPostulate (tset --> tset))
  , (builtinFFIFunImportSpec   |-> BuiltinRecord tset (replicate 3 tffifunimp))
  , (builtinFFIFunImport       |-> BuiltinData tset [builtinFFIFunImport_MAZ_HS, builtinFFIFunImport_JS_JS
                                                    , builtinFFIFunImport_UHC_Core])
  , (builtinFFIFunImport_MAZ_HS   |-> BuiltinDataCons (tstring --> tffifunimp))
  , (builtinFFIFunImport_JS_JS    |-> BuiltinDataCons (tstring --> tffifunimp))
  , (builtinFFIFunImport_UHC_Core |-> BuiltinDataCons (tstring --> tffifunimp))
  , (builtinFFIFunImport_RuntimeError |-> BuiltinDataCons tffifunimp)
  , (builtinAgdaSort           |-> BuiltinData tset [builtinAgdaSortSet, builtinAgdaSortLit, builtinAgdaSortUnsupported])
  , (builtinAgdaType           |-> BuiltinData tset [builtinAgdaTypeEl])
  , (builtinAgdaTerm           |-> BuiltinData tset
                                     [ builtinAgdaTermVar, builtinAgdaTermLam, builtinAgdaTermExtLam
                                     , builtinAgdaTermDef, builtinAgdaTermCon
                                     , builtinAgdaTermPi, builtinAgdaTermSort
                                     , builtinAgdaTermLit, builtinAgdaTermQuoteTerm, builtinAgdaTermQuoteGoal
                                     , builtinAgdaTermQuoteContext, builtinAgdaTermUnquote, builtinAgdaTermForeign
                                     , builtinAgdaTermUnsupported])
  , (builtinEquality           |-> BuiltinData (hPi "a" (el primLevel) $
                                                hPi "A" (return $ sort $ varSort 0) $
                                                (El (varSort 1) <$> varM 0) -->
                                                (El (varSort 1) <$> varM 0) -->
                                                return (sort $ varSort 1))
                                               [builtinRefl])
  , (builtinHiding             |-> BuiltinData tset [builtinHidden, builtinInstance, builtinVisible])
  , (builtinRelevance          |-> BuiltinData tset [builtinRelevant, builtinIrrelevant])
  , (builtinRefl               |-> BuiltinDataCons (hPi "a" (el primLevel) $
                                                    hPi "A" (return $ sort $ varSort 0) $
                                                    hPi "x" (El (varSort 1) <$> varM 0) $
                                                    El (varSort 2) <$> primEquality <#> varM 2 <#> varM 1 <@> varM 0 <@> varM 0))
  , (builtinRewrite           |-> BuiltinUnknown Nothing verifyBuiltinRewrite)
  , (builtinNil                |-> BuiltinDataCons (hPi "A" tset (el (list v0))))
  , (builtinCons               |-> BuiltinDataCons (hPi "A" tset (tv0 --> el (list v0) --> el (list v0))))
  , (builtinZero               |-> BuiltinDataCons tnat)
  , (builtinSuc                |-> BuiltinDataCons (tnat --> tnat))
  , (builtinTrue               |-> BuiltinDataCons tbool)
  , (builtinFalse              |-> BuiltinDataCons tbool)
  , (builtinArgArg             |-> BuiltinDataCons (hPi "A" tset (targinfo --> tv0 --> targ tv0)))
  , (builtinAbsAbs             |-> BuiltinDataCons (hPi "A" tset (tstring  --> tv0 --> tabs tv0)))
  , (builtinArgArgInfo         |-> BuiltinDataCons (thiding --> trelevance --> targinfo))
  , (builtinAgdaTypeEl         |-> BuiltinDataCons (tsort --> tterm --> ttype))
  , (builtinAgdaTermVar        |-> BuiltinDataCons (tnat --> targs --> tterm))
  , (builtinAgdaTermLam        |-> BuiltinDataCons (thiding --> tabs tterm --> tterm))
  , (builtinAgdaTermExtLam     |-> BuiltinDataCons (tlist tclause --> targs --> tterm))
  , (builtinAgdaTermDef        |-> BuiltinDataCons (tqname --> targs --> tterm))
  , (builtinAgdaTermCon        |-> BuiltinDataCons (tqname --> targs --> tterm))
  , (builtinAgdaTermPi         |-> BuiltinDataCons (targ ttype --> tabs ttype --> tterm))
  , (builtinAgdaTermSort       |-> BuiltinDataCons (tsort --> tterm))
  , (builtinAgdaTermLit        |-> BuiltinDataCons (tliteral --> tterm))
  , (builtinAgdaTermQuoteGoal    |-> BuiltinDataCons (tabs tterm --> tterm))
  , (builtinAgdaTermQuoteTerm    |-> BuiltinDataCons (tterm --> tterm))
  , (builtinAgdaTermQuoteContext |-> BuiltinDataCons tterm)
  , (builtinAgdaTermUnquote      |-> BuiltinDataCons (tterm --> targs --> tterm))
  , (builtinAgdaTermForeign      |-> BuiltinDataCons (tterm --> ttype --> tterm))
  , (builtinAgdaTermUnsupported|-> BuiltinDataCons tterm)
  , (builtinAgdaLitNat    |-> BuiltinDataCons (tnat --> tliteral))
  , (builtinAgdaLitFloat  |-> BuiltinDataCons (tfloat --> tliteral))
  , (builtinAgdaLitChar   |-> BuiltinDataCons (tchar --> tliteral))
  , (builtinAgdaLitString |-> BuiltinDataCons (tstring --> tliteral))
  , (builtinAgdaLitQName  |-> BuiltinDataCons (tqname --> tliteral))
  , (builtinHidden             |-> BuiltinDataCons thiding)
  , (builtinInstance           |-> BuiltinDataCons thiding)
  , (builtinVisible            |-> BuiltinDataCons thiding)
  , (builtinRelevant           |-> BuiltinDataCons trelevance)
  , (builtinIrrelevant         |-> BuiltinDataCons trelevance)
  , (builtinSizeUniv           |-> builtinPostulate tSizeUniv) -- SizeUniv : SizeUniv
-- See comment on tSizeUniv: the following does not work currently.
--  , (builtinSizeUniv           |-> builtinPostulate tSetOmega) -- SizeUniv : Setω
  , (builtinSize               |-> builtinPostulate tSizeUniv)
  , (builtinSizeLt             |-> builtinPostulate (tsize ..--> tSizeUniv))
  , (builtinSizeSuc            |-> builtinPostulate (tsize --> tsize))
  , (builtinSizeInf            |-> builtinPostulate tsize)
  -- postulate max : {i : Size} -> Size< i -> Size< i -> Size< i
  , (builtinSizeMax            |-> builtinPostulate (tsize --> tsize --> tsize))
     -- (hPi "i" tsize $ let a = el $ primSizeLt <@> v0 in (a --> a --> a)))
  -- postulate .irrelevant : {a : Level}{A : Set a} -> .A -> A
  , (builtinIrrAxiom           |-> BuiltinPostulate Irrelevant
                                     (hPi "a" (el primLevel) $ hPi "A" (return $ sort $ varSort 0) $
                                      (El (varSort 1) <$> varM 0) .--> (El (varSort 1) <$> varM 0)))
  , (builtinAgdaSortSet        |-> BuiltinDataCons (tterm --> tsort))
  , (builtinAgdaSortLit        |-> BuiltinDataCons (tnat --> tsort))
  , (builtinAgdaSortUnsupported|-> BuiltinDataCons tsort)
  , (builtinNatPlus            |-> BuiltinPrim "primNatPlus" verifyPlus)
  , (builtinNatMinus           |-> BuiltinPrim "primNatMinus" verifyMinus)
  , (builtinNatTimes           |-> BuiltinPrim "primNatTimes" verifyTimes)
  , (builtinNatDivSucAux       |-> BuiltinPrim "primNatDivSucAux" verifyDivSucAux)
  , (builtinNatModSucAux       |-> BuiltinPrim "primNatModSucAux" verifyModSucAux)
  , (builtinNatEquals          |-> BuiltinPrim "primNatEquality" verifyEquals)
  , (builtinNatLess            |-> BuiltinPrim "primNatLess" verifyLess)
  , (builtinLevelZero          |-> BuiltinPrim "primLevelZero" (const $ return ()))
  , (builtinLevelSuc           |-> BuiltinPrim "primLevelSuc" (const $ return ()))
  , (builtinLevelMax           |-> BuiltinPrim "primLevelMax" verifyMax)
  , (builtinAgdaFunDef         |-> BuiltinData tset [builtinAgdaFunDefCon])
  , (builtinAgdaFunDefCon      |-> BuiltinDataCons (ttype --> tlist tclause --> tfun))
  , (builtinAgdaClause         |-> BuiltinData tset [builtinAgdaClauseClause, builtinAgdaClauseAbsurd])
  , (builtinAgdaClauseClause   |-> BuiltinDataCons (tlist (targ tpat) --> tterm --> tclause))
  , (builtinAgdaClauseAbsurd   |-> BuiltinDataCons (tlist (targ tpat) --> tclause))
  , (builtinAgdaDataDef               |-> builtinPostulate tset) -- internally this is QName
  , (builtinAgdaRecordDef             |-> builtinPostulate tset) -- internally this is QName
  , (builtinAgdaDefinition            |-> BuiltinData tset [builtinAgdaDefinitionFunDef
                                                           ,builtinAgdaDefinitionDataDef
                                                           ,builtinAgdaDefinitionDataConstructor
                                                           ,builtinAgdaDefinitionRecordDef
                                                           ,builtinAgdaDefinitionPostulate
                                                           ,builtinAgdaDefinitionPrimitive])
  , (builtinAgdaDefinitionFunDef          |-> BuiltinDataCons (tfun --> tdefn))
  , (builtinAgdaDefinitionDataDef         |-> BuiltinDataCons (tdtype --> tdefn))
  , (builtinAgdaDefinitionDataConstructor |-> BuiltinDataCons tdefn)
  , (builtinAgdaDefinitionRecordDef       |-> BuiltinDataCons (trec --> tdefn))
  , (builtinAgdaDefinitionPostulate       |-> BuiltinDataCons tdefn)
  , (builtinAgdaDefinitionPrimitive       |-> BuiltinDataCons tdefn)
  ]
  where
        (|->) = (,)

        v0 = varM 0
        v1 = varM 1

        tv0,tv1 :: TCM Type
        tv0 = el v0
        tv1 = el v1

        arg :: TCM Term -> TCM Term
        arg t = primArg <@> t

        tlist x    = el $ list (fmap unEl x)
        targ x     = el (arg (fmap unEl x))
        tabs x     = el (primAbs <@> fmap unEl x)
        targs      = el (list (arg primAgdaTerm))
        tterm      = el primAgdaTerm
        tnat       = el primNat
        tfloat     = el primFloat
        tchar      = el primChar
        tstring    = el primString
        tqname     = el primQName
        tsize      = El SizeUniv <$> primSize
        tbool      = el primBool
        tffifunimp = el primFFIFunImport
        thiding    = el primHiding
        trelevance = el primRelevance
--        tcolors    = el (list primAgdaTerm) -- TODO guilhem
        targinfo   = el primArgInfo
        ttype      = el primAgdaType
        tsort      = el primAgdaSort
        tdefn      = el primAgdaDefinition
        tfun       = el primAgdaFunDef
        tdtype     = el primAgdaDataDef
        trec       = el primAgdaRecordDef
        tliteral   = el primAgdaLiteral
        tpat       = el primAgdaPattern
        tclause    = el primAgdaClause

        verifyPlus plus =
            verify ["n","m"] $ \(@@) zero suc (==) (===) choice -> do
                let m = var 0
                    n = var 1
                    x + y = plus @@ x @@ y

                -- We allow recursion on any argument
                choice
                    [ do n + zero  == n
                         n + suc m == suc (n + m)
                    , do suc n + m == suc (n + m)
                         zero  + m == m
                    ]

        verifyMinus minus =
            verify ["n","m"] $ \(@@) zero suc (==) (===) choice -> do
                let m = var 0
                    n = var 1
                    x - y = minus @@ x @@ y

                -- We allow recursion on any argument
                zero  - zero  == zero
                zero  - suc m == zero
                suc n - zero  == suc n
                suc n - suc m == (n - m)

        verifyTimes times = do
            plus <- primNatPlus
            verify ["n","m"] $ \(@@) zero suc (==) (===) choice -> do
                let m = var 0
                    n = var 1
                    x + y = plus  @@ x @@ y
                    x * y = times @@ x @@ y

                choice
                    [ do n * zero == zero
                         choice [ (n * suc m) == (n + (n * m))
                                , (n * suc m) == ((n * m) + n)
                                ]
                    , do zero * n == zero
                         choice [ (suc n * m) == (m + (n * m))
                                , (suc n * m) == ((n * m) + m)
                                ]
                    ]

        verifyDivSucAux dsAux =
            verify ["k","m","n","j"] $ \(@@) zero suc (==) (===) choice -> do
                let aux k m n j = dsAux @@ k @@ m @@ n @@ j
                    k           = var 0
                    m           = var 1
                    n           = var 2
                    j           = var 3

                aux k m zero    j       == k
                aux k m (suc n) zero    == aux (suc k) m n m
                aux k m (suc n) (suc j) == aux k m n j

        verifyModSucAux dsAux =
            verify ["k","m","n","j"] $ \(@@) zero suc (==) (===) choice -> do
                let aux k m n j = dsAux @@ k @@ m @@ n @@ j
                    k           = var 0
                    m           = var 1
                    n           = var 2
                    j           = var 3

                aux k m zero    j       == k
                aux k m (suc n) zero    == aux zero m n m
                aux k m (suc n) (suc j) == aux (suc k) m n j

        verifyEquals eq =
            verify ["n","m"] $ \(@@) zero suc (==) (===) choice -> do
            true  <- primTrue
            false <- primFalse
            let x == y = eq @@ x @@ y
                m      = var 0
                n      = var 1
            (zero  == zero ) === true
            (suc n == suc m) === (n == m)
            (suc n == zero ) === false
            (zero  == suc n) === false

        verifyLess leq =
            verify ["n","m"] $ \(@@) zero suc (==) (===) choice -> do
            true  <- primTrue
            false <- primFalse
            let x < y = leq @@ x @@ y
                m     = var 0
                n     = var 1
            (n     < zero)  === false
            (suc n < suc m) === (n < m)
            (zero  < suc m) === true

        verifyMax maxV = return ()  -- TODO: make max a postulate

        verify xs = verify' primNat primZero primSuc xs

        verify' ::  TCM Term -> TCM Term -> TCM Term ->
                    [String] -> ( (Term -> Term -> Term) -> Term -> (Term -> Term) ->
                                (Term -> Term -> TCM ()) ->
                                (Term -> Term -> TCM ()) ->
                                ([TCM ()] -> TCM ()) -> TCM a) -> TCM a
        verify' pNat pZero pSuc xs f = do
            nat  <- El (mkType 0) <$> pNat
            zero <- pZero
            s    <- pSuc
            let x @@ y  = x `apply` [defaultArg y]
                x == y  = noConstraints $ equalTerm nat x y
                -- Andreas: 2013-10-21 I put primBool here on the inside
                -- since some Nat-builtins do not require Bool-builtins
                x === y = do bool <- El (mkType 0) <$> primBool
                             noConstraints $ equalTerm bool x y
                suc n  = s @@ n
                choice = foldr1 (\x y -> x `catchError` \_ -> y)
            xs <- mapM freshName_ xs
            addCtxs xs (domFromArg $ defaultArg nat) $ f (@@) zero suc (==) (===) choice


inductiveCheck :: String -> Int -> Term -> TCM ()
inductiveCheck b n t = do
    t <- etaContract =<< normalise t
    let err = typeError (NotInductive t)
    case ignoreSharing t of
      Def t _ -> do
        t <- theDef <$> getConstInfo t
        case t of
          Datatype { dataInduction = Inductive
                   , dataCons      = cs
                   }
            | length cs == n -> return ()
            | otherwise ->
              typeError $ GenericError $ unwords
                          [ "The builtin", b
                          , "must be a datatype with", show n
                          , "constructors" ]
          _ -> err
      _ -> err

recordCheck :: String -> Term -> TCM ()
recordCheck b t = do
  t <- etaContract =<< normalise t
  case ignoreSharing t of
    Def t _ -> do
      t <- theDef <$> getConstInfo t
      case t of
        Record {} -> return ()
        _ -> typeError $ GenericError $ unwords
            [ "The builtin", b
            , "must be a record type."]
    _ -> error $ show t --__IMPOSSIBLE__


-- | @bindPostulatedName builtin e m@ checks that @e@ is a postulated
-- name @q@, and binds the builtin @builtin@ to the term @m q def@,
-- where @def@ is the current 'Definition' of @q@.

bindPostulatedName ::
  String -> A.Expr -> (QName -> Definition -> TCM Term) -> TCM ()
bindPostulatedName builtin e m = do
  q   <- getName e
  def <- ignoreAbstractMode $ getConstInfo q
  case theDef def of
    Axiom {} -> bindBuiltinName builtin =<< m q def
    _        -> err
  where
  err = typeError $ GenericError $
          "The argument to BUILTIN " ++ builtin ++
          " must be a postulated name"

  getName (A.Def q)          = return q
  getName (A.ScopedExpr _ e) = getName e
  getName _                  = err

getDef :: Term -> TCM QName
getDef t = do
  t <- etaContract =<< normalise t
  case ignoreSharing t of
    Def d _ -> return d
    _ -> __IMPOSSIBLE__

bindBuiltinBool :: Term -> TCM ()
bindBuiltinBool t = do
  bool <- getDef t
  addFFITypeBind bool Way_MAZ_HS (TyBind_MAZ_HS "Bool")
  bindBuiltinName builtinBool t

bindBuiltinNat :: Term -> TCM ()
bindBuiltinNat t = do
  nat <- getDef t
  def <- theDef <$> getConstInfo nat
  case def of
    Datatype { dataCons = [c1, c2] } -> do
      bindBuiltinName builtinNat t
      let getArity c = arity <$> (normalise . defType =<< getConstInfo c)
      [a1, a2] <- mapM getArity [c1, c2]
      let (zero, suc) | a2 > a1   = (c1, c2)
                      | otherwise = (c2, c1)
          tnat = el primNat
          rerange = setRange (getRange nat)
      addFFITypeBind nat Way_MAZ_HS (TyBind_MAZ_HS "Integer")
      bindBuiltinInfo (BuiltinInfo builtinZero $ BuiltinDataCons tnat)
                      (A.Con $ AmbQ [rerange zero])
      bindBuiltinInfo (BuiltinInfo builtinSuc  $ BuiltinDataCons (tnat --> tnat))
                      (A.Con $ AmbQ [rerange suc])
    _ -> __IMPOSSIBLE__

bindBuiltinInfo :: BuiltinInfo -> A.Expr -> TCM ()
bindBuiltinInfo i (A.ScopedExpr scope e) = setScope scope >> bindBuiltinInfo i e
bindBuiltinInfo (BuiltinInfo s d) e = do
    case d of
      BuiltinData t cs -> do
        e' <- checkExpr e =<< t
        let n = length cs
        inductiveCheck s n e'
        case () of
          _ | s == builtinBool -> bindBuiltinBool e'
            | s == builtinNat  -> bindBuiltinNat e'
            | otherwise        -> bindBuiltinName s e'

      BuiltinDataCons t -> do

        let name (Lam h b)  = name (absBody b)
            name (Con c _)  = Con c []
            name (Shared p) = name $ ignoreSharing (derefPtr p)
            name _          = __IMPOSSIBLE__

        e' <- checkExpr e =<< t

        case e of
          A.Con _ -> return ()
          _       -> typeError $ BuiltinMustBeConstructor s e

        let v@(Con h []) = name e'
            c = conName h
        when (s == builtinTrue)  $ addFFIConBind c Way_MAZ_HS (ConBind_MAZ_HS "Bool" "True")
        when (s == builtinFalse) $ addFFIConBind c Way_MAZ_HS (ConBind_MAZ_HS "Bool" "False")
        bindBuiltinName s v

      BuiltinRecord t fs -> do
        e' <- checkExpr e =<< t
        recordCheck s e'
        bindBuiltinName s e'


      BuiltinPrim pfname axioms -> do
        case e of
          A.Def qx -> do

            PrimImpl t pf <- lookupPrimitiveFunction pfname
            v <- checkExpr e t

            axioms v

            info <- getConstInfo qx
            let cls = defClauses info
                a   = defAbstract info
                mcc = defCompiled info
            bindPrimitive pfname $ pf { primFunName = qx }
            addConstant qx $ info { theDef = Primitive a pfname cls mcc }

            -- needed? yes, for checking equations for mul
            bindBuiltinName s v

          _ -> typeError $ GenericError $ "Builtin " ++ s ++ " must be bound to a function"

      BuiltinPostulate rel t -> do
        t' <- t
        e' <- applyRelevanceToContext rel $ checkExpr e t'
        let err = typeError $ GenericError $
                    "The argument to BUILTIN " ++ s ++ " must be a postulated name"
        case e of
          A.Def q -> do
            def <- ignoreAbstractMode $ getConstInfo q
            case theDef def of
              Axiom {} -> do
                builtinSizeHook s q t'
                bindBuiltinName s e'
              _        -> err
          _ -> err

      BuiltinUnknown mt f -> do
        (v, t) <- caseMaybe mt (inferExpr e) $ \ tcmt -> do
          t <- tcmt
          (,t) <$> checkExpr e t
        f v t
        bindBuiltinName s v

-- | Bind a builtin thing to an expression.
bindBuiltin :: String -> A.Expr -> TCM ()
bindBuiltin b e = do
  unlessM ((0 ==) <$> getContextSize) $ typeError $ BuiltinInParameterisedModule b
  case b of
    _ | b == builtinZero  -> nowNat b
    _ | b == builtinSuc   -> nowNat b
    _ | b == builtinInf   -> bindBuiltinInf e
    _ | b == builtinSharp -> bindBuiltinSharp e
    _ | b == builtinFlat  -> bindBuiltinFlat e
    _ | Just i <- find ((b ==) . builtinName) coreBuiltins -> bindBuiltinInfo i e
    _ -> typeError $ NoSuchBuiltinName b
  where
    nowNat b = genericError $
      "Builtin " ++ b ++ " does no longer exist. " ++
      "It is now bound by BUILTIN " ++ builtinNat

isUntypedBuiltin :: String -> Bool
isUntypedBuiltin b = elem b [builtinFromNat, builtinFromNeg]

bindUntypedBuiltin :: String -> A.Expr -> TCM ()
bindUntypedBuiltin b e =
  case A.unScope e of
    A.Def q  -> bindBuiltinName b (Def q [])
    A.Proj q -> bindBuiltinName b (Def q [])
    e        -> genericError $ "The argument to BUILTIN " ++ b ++ " must be a defined name"

-- | Bind a builtin thing to a new name.
bindBuiltinNoDef :: String -> A.QName -> TCM ()
bindBuiltinNoDef b q = do
  unlessM ((0 ==) <$> getContextSize) $ typeError $ BuiltinInParameterisedModule b
  case lookup b $ map (\ (BuiltinInfo b i) -> (b, i)) coreBuiltins of
    Just (BuiltinPostulate rel mt) -> do
      t <- mt
      addConstant q $ defaultDefn (setRelevance rel defaultArgInfo) q t def
      builtinSizeHook b q t
      bindBuiltinName b $ Def q []
      where
        -- Andreas, 2015-02-14
        -- Special treatment of SizeUniv, should maybe be a primitive.
        def | b == builtinSizeUniv = emptyFunction
                { funClauses = [ (empty :: Clause) { clauseBody = Body $ Sort SizeUniv } ]
                , funCompiled = Just (CC.Done [] $ Sort SizeUniv)
                , funTerminates = Just True
                }
            | otherwise = Axiom

    Just{}  -> __IMPOSSIBLE__
    Nothing -> __IMPOSSIBLE__ -- typeError $ NoSuchBuiltinName b
