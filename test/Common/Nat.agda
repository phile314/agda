module Common.Nat where

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

{-# BUILTIN NATURAL Nat  #-}

{-# COMPILED_JS     Nat function (x,v) { return (x < 1? v.zero(): v.suc(x-1)); } #-}
{-# COMPILED_JS     zero 0 #-}
{-# COMPILED_JS     suc function (x) { return x+1; } #-}

infixl 7 _*_
infixl 6 _+_ _∸_

_+_ : Nat → Nat → Nat
zero  + n = n
suc m + n = suc (m + n)

{-# BUILTIN NATPLUS _+_ #-}

_*_ : Nat → Nat → Nat
zero  * n = zero
suc m * n = n + m * n

{-# BUILTIN NATTIMES _*_ #-}

_∸_ : Nat → Nat → Nat
m     ∸ zero  = m
zero  ∸ _     = zero
suc m ∸ suc n = m ∸ n

{-# BUILTIN NATMINUS _∸_ #-}

pred : Nat → Nat
pred zero    = zero
pred (suc n) = n

