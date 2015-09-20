module Common.FFI where

open import Common.String
open import Common.Reflection

JSCode : Set
JSCode = String

HSCode : Set
HSCode = String

UHC-Core-Expr UHC-HS-Name : Set
UHC-Core-Expr = String
UHC-HS-Name = String


data FunImport : Set where
  MAZ-HS : HSCode -> FunImport
  JS-JS : JSCode -> FunImport
  UHC-Core : UHC-HS-Name -> FunImport
  RuntimeError : FunImport
--  UHC-Core : UHC-Core-Expr -> FunImport
{-# BUILTIN FFIFUNIMPORT FunImport #-}
{-# BUILTIN FFIFUNIMPORTMAZHS Imp-MAZ-HS #-}
{-# BUILTIN FFIFUNIMPORTJSJS  Imp-JS-JS #-}
{-# BUILTIN FFIFUNIMPORTUHCCORE Imp-UHC-Core #-}
{-# BUILTIN FFIFUNIMPORTRUNTIMEERROR RuntimeError #-}
-- {-# BUILTIN FFIFUNIMPORTUHCCORE UHC-Core #-}


record FunImportSpec : Set where
  constructor FFICall
  field
    Spec-MAZ-HS Spec-JS-JS Spec-UHC-Core : FunImport
{-# BUILTIN FFIFUNIMPORTSPEC FunImportSpec #-}
