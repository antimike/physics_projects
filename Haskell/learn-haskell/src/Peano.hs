{- Example implementations of Peano numbers
See https://wiki.haskell.org/Peano_numbers -}

module Peano where

{- "Values"-style encoding -}
data Peano = Zero | Succ Peano

add :: Peano -> Peano
add Zero b = b
add (Succ a) b = Succ (add a b)

{- "Number types"-style encoding using type arithmetic -}

data ZeroT
data SuccT a
class AddT a b ab | a b -> ab, a ab -> b
instance AddT ZeroT b b
instance (AddT a b ab) => Add (SuccT a) b (SuccT ab)
