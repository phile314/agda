module NewFFI where


open import Common.IO
open import Common.Unit
open import Common.String
open import Common.Char
open import Common.FFI
open import Common.Level
open import Common.List

data LList {l} (A : Set l) : Set l where
  [] : LList A
  _∷_ : A → LList A → LList A

{-# COMPILED_DATA LList [] [] (:) #-}
{-# COMPILED_DATA_UHC LList __LIST__ __NIL__ __CONS__ #-}


hhead : {-(a : Level) ->-} {A : Set0} -> LList A -> A
hhead = foreign (record
    { Spec-MAZ-HS = RuntimeError
    ; Spec-JS-JS = RuntimeError
    ; Spec-UHC-HS = UHC-Core "(\\_ -> UHC.Base.head)"}) ({A : Set0} -> LList A -> A)

str : LList Char
str = 'a' ∷ ('b' ∷ ('c' ∷ ('d' ∷ [])))

main : IO Unit
main = putStr (charToStr (hhead str))
