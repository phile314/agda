module NewFFI where


open import Common.IO
open import Common.Unit
open import Common.String
open import Common.Char
open import Common.FFI
open import Common.Level
open import Common.List
open import Common.Bool

data LList {l} (A : Set l) : Set l where
  [] : LList A
  _∷_ : A → LList A → LList A

{-# COMPILED_DATA LList [] [] (:) #-}
{-# COMPILED_DATA_UHC LList __LIST__ __NIL__ __CONS__ #-}


hhead : {a : Level} -> {A : Set a} -> LList A -> A
hhead = foreign (record
    { Spec-MAZ-Native = RuntimeError
    ; Spec-JS-JS = RuntimeError
--    ; Spec-UHC-Native = UHC-Core "(\\_ -> UHC.Base.head)"})
    ; Spec-UHC-Native = UHC-HS "UHC.Base.head"
    })
    ({a : Level} -> {A : Set a} -> LList A -> A)


test = foreign (record
    { Spec-MAZ-Native = RuntimeError
    ; Spec-JS-JS = RuntimeError
    ; Spec-UHC-Native = UHC-HS "UHC.Base.head"
    })
    (({a : Level} -> {A : Set a} -> A -> A) -> Bool)

--postulate hhead : {A : Set0} -> LList A -> A
-- {-# COMPILED_UHC hhead (\_ -> UHC.Base.head) #-}

str : LList Char
str = 'a' ∷ ('b' ∷ ('c' ∷ ('d' ∷ [])))

main : IO Unit
main = putStr (charToStr (hhead str))
