{-# OPTIONS_GHC -fno-warn-orphans -fwarn-unused-imports #-}
module Agda.TypeChecking.Serialise.Instances.Compilers where

import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B

import qualified Agda.Compiler.Epic.Interface as Epic
import qualified Agda.Compiler.UHC.Pragmas.Base as CR
import qualified Agda.Compiler.UHC.Bridge as UHCB
import qualified Agda.Compiler.JS.Syntax as JS

import Agda.TypeChecking.Serialise.Base
import Agda.TypeChecking.Serialise.Instances.Common ()

import Agda.TypeChecking.Monad

instance EmbPrj CompileTarget where
  icod_ Target_MAZ_Native = icode0 0
  icod_ Target_JS_JS      = icode0 1
  icod_ Target_UHC_Native = icode0 2
  value = vcase valu where
    valu [0] = valu0 Target_MAZ_Native
    valu [1] = valu0 Target_JS_JS
    valu [2] = valu0 Target_UHC_Native
    valu _   = malformed

instance EmbPrj FFIWay where
  icod_ Way_MAZ_HS = icode0 0
  icod_ Way_JS_JS  = icode0 1
  icod_ Way_UHC_Core = icode0 2
  icod_ Way_UHC_HS = icode0 3
  value = vcase valu where
    valu [0] = valu0 Way_MAZ_HS
    valu [1] = valu0 Way_JS_JS
    valu [2] = valu0 Way_UHC_Core
    valu [3] = valu0 Way_UHC_HS
    valu _   = malformed


instance EmbPrj FFIFunImport' where
  icod_ (FunImp_MAZ_HS a)   = icode1 0 a
  icod_ (FunImp_JS_JS a)    = icode1 1 a
  icod_ (FunImp_UHC_HS a)   = icode1 2 a
  icod_ (FunImp_UHC_Core a) = icode1 3 a
  value = vcase valu where
    valu [0, a]    = valu1 FunImp_MAZ_HS a
    valu [1, a]    = valu1 FunImp_JS_JS a
    valu [2, a]    = valu1 FunImp_UHC_HS a
    valu [3, a]    = valu1 FunImp_UHC_Core a
    valu _         = malformed

instance EmbPrj FFITypeBind' where
  icod_ (TyBind_MAZ_HS a) = icode1 0 a
  icod_ (TyBind_JS_JS a)  = icode1 1 a
  icod_ (TyBind_UHC_Core a) = icode1 2 a
  value = vcase valu where
    valu [0, a] = valu1 TyBind_MAZ_HS a
    valu [1, a] = valu1 TyBind_JS_JS a
    valu [2, a] = valu1 TyBind_UHC_Core a
    valu _      = malformed

instance EmbPrj FFIFunExport' where
  icod_ (FunExp_MAZ_HS a b) = icode2 0 a b
  value = vcase valu where
    valu [0, a, b] = valu2 FunExp_MAZ_HS a b
    valu _         = malformed

instance EmbPrj FFIConBind' where
  icod_ (ConBind_MAZ_HS a b) = icode2 0 a b
  icod_ (ConBind_JS_JS a)    = icode1 1 a
  icod_ (ConBind_UHC_Core a) = icode1 2 a
  value = vcase valu where
    valu [0, a, b] = valu2 ConBind_MAZ_HS a b
    valu [1, a]    = valu1 ConBind_JS_JS a
    valu [2, a]    = valu1 ConBind_UHC_Core a
    valu _         = malformed


instance EmbPrj CompiledRepresentation where
  icod_ (FFIFunImport a) = icode1 0 a
  icod_ (FFIFunExport a) = icode1 1 a
  icod_ (FFITypeBind a)  = icode1 2 a
  icod_ (FFIConBind a)   = icode1 3 a
  icod_ (FFINoBind)      = icode0 4
  value = vcase valu where
    valu [0, a] = valu1 FFIFunImport a
    valu [1, a] = valu1 FFIFunExport a
    valu [2, a] = valu1 FFITypeBind a
    valu [3, a] = valu1 FFIConBind a
    valu [4]    = valu0 FFINoBind
    valu _      = malformed




instance EmbPrj JS.Exp where
  icod_ (JS.Self)         = icode0 0
  icod_ (JS.Local i)      = icode1 1 i
  icod_ (JS.Global i)     = icode1 2 i
  icod_ (JS.Undefined)    = icode0 3
  icod_ (JS.String s)     = icode1 4 s
  icod_ (JS.Char c)       = icode1 5 c
  icod_ (JS.Integer n)    = icode1 6 n
  icod_ (JS.Double d)     = icode1 7 d
  icod_ (JS.Lambda n e)   = icode2 8 n e
  icod_ (JS.Object o)     = icode1 9 o
  icod_ (JS.Apply e es)   = icode2 10 e es
  icod_ (JS.Lookup e l)   = icode2 11 e l
  icod_ (JS.If e f g)     = icode3 12 e f g
  icod_ (JS.BinOp e op f) = icode3 13 e op f
  icod_ (JS.PreOp op e)   = icode2 14 op e
  icod_ (JS.Const i)      = icode1 15 i
  value = vcase valu where valu [0]           = valu0 JS.Self
                           valu [1,  a]       = valu1 JS.Local a
                           valu [2,  a]       = valu1 JS.Global a
                           valu [3]           = valu0 JS.Undefined
                           valu [4,  a]       = valu1 JS.String a
                           valu [5,  a]       = valu1 JS.Char a
                           valu [6,  a]       = valu1 JS.Integer a
                           valu [7,  a]       = valu1 JS.Double a
                           valu [8,  a, b]    = valu2 JS.Lambda a b
                           valu [9,  a]       = valu1 JS.Object a
                           valu [10, a, b]    = valu2 JS.Apply a b
                           valu [11, a, b]    = valu2 JS.Lookup a b
                           valu [12, a, b, c] = valu3 JS.If a b c
                           valu [13, a, b, c] = valu3 JS.BinOp a b c
                           valu [14, a, b]    = valu2 JS.PreOp a b
                           valu [15, a]       = valu1 JS.Const a
                           valu _             = malformed

instance EmbPrj JS.LocalId where
  icod_ (JS.LocalId l) = icode l
  value n = JS.LocalId `fmap` value n

instance EmbPrj JS.GlobalId where
  icod_ (JS.GlobalId l) = icode l
  value n = JS.GlobalId `fmap` value n

instance EmbPrj JS.MemberId where
  icod_ (JS.MemberId l) = icode l
  value n = JS.MemberId `fmap` value n

{-
instance EmbPrj CoreRepresentation where
  icod_ (CrDefn a)   = icode1 1 a
  icod_ (CrType a)   = icode1 2 a
  icod_ (CrConstr a) = icode1 3 a

  value = vcase valu where
    valu [1, a] = valu1 CrDefn a
    valu [2, a] = valu1 CrType a
    valu [3, a] = valu1 CrConstr a
    valu _      = malformed
-}

instance EmbPrj CR.CoreType where
  icod_ (CR.CTMagic a) = icode1 1 a
  icod_ (CR.CTNormal a)  = icode1 2 a
  value = vcase valu where
    valu [1, a]    = valu1 CR.CTMagic a
    valu [2, a]    = valu1 CR.CTNormal a
    valu _         = malformed

instance EmbPrj CR.CoreConstr where
  icod_ (CR.CCMagic a b) = icode2 1 a b
  icod_ (CR.CCNormal a b c) = icode3 2 a b c
  value = vcase valu where
    valu [1, a, b]    = valu2 CR.CCMagic a b
    valu [2, a, b, c] = valu3 CR.CCNormal a b c
    valu _            = malformed

instance EmbPrj CR.HsName where
  icod_ = icode . B.runPut . UHCB.serialize
  value n = value n >>= return . (B.runGet UHCB.unserialize)

-- This is used for the Epic compiler backend
instance EmbPrj Epic.EInterface where
  icod_ (Epic.EInterface a b c d e f g h) = icode8' a b c d e f g h
  value = vcase valu where
    valu [a, b, c, d, e, f, g, h] = valu8 Epic.EInterface a b c d e f g h
    valu _                        = malformed

instance EmbPrj Epic.InjectiveFun where
  icod_ (Epic.InjectiveFun a b) = icode2' a b
  value = vcase valu where
     valu [a,b] = valu2 Epic.InjectiveFun a b
     valu _     = malformed

instance EmbPrj Epic.Relevance where
  icod_ Epic.Irr      = icode0 0
  icod_ Epic.Rel      = icode0 1
  value = vcase valu where valu [0] = valu0 Epic.Irr
                           valu [1] = valu0 Epic.Rel
                           valu _   = malformed

instance EmbPrj Epic.Forced where
  icod_ Epic.Forced    = icode0 0
  icod_ Epic.NotForced = icode0 1
  value = vcase valu where valu [0] = valu0 Epic.Forced
                           valu [1] = valu0 Epic.NotForced
                           valu _   = malformed

instance EmbPrj Epic.Tag where
  icod_ (Epic.Tag a)     = icode1 0 a
  icod_ (Epic.PrimTag a) = icode1 1 a
  value = vcase valu
    where
    valu [0, a] = valu1 Epic.Tag a
    valu [1, a] = valu1 Epic.PrimTag a
    valu _      = malformed

