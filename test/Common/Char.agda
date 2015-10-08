module Common.Char where

open import Common.Bool

postulate
  Char : Set

{-# BUILTIN CHAR Char #-}
{-# COMPILED_TYPE Char Char #-}
{-# COMPILED_TYPE_UHC Char #-}

private
  primitive
    primCharEquality : (c c' : Char) -> Bool


charEq : Char -> Char -> Bool
charEq = primCharEquality

