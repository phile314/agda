{-# OPTIONS --without-K #-}

module Agda.Builtin.IO where

postulate IO : ∀ {a} → Set a → Set a
{-# BUILTIN IO IO #-}

{-# FOREIGN GHC type AgdaIO a b = IO b #-}
{-# COMPILE GHC IO = type MAlonzo.Code.Agda.Builtin.IO.AgdaIO #-}
