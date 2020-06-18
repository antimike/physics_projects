{- Examples illustrating the Haskell pattern of "tying the knot" to create
cyclic data structures recursively
See https://wiki.haskell.org/Tying_the_Knot
Note that Peano numbers can be used to control laziness with this technique -}

module Knot where

-- First example: doubly-linked circular list
data DList a = DLNode (DList a) a (DList a)

-- Conversion fn from ordinary Haskell list
mkDList :: [a] -> DList a

mkDList [] = error "must have at least one element"
mkDList xs = let (first, last) = go last xs first
             in  first

  where go :: DList a -> [a] -> DList a -> (DList a, DList a)
        go prev []     next = (next, prev)
        go prev (x:xs) next = let this         = DLNode prev x rest
                                  (rest, last) = go this xs next
                              in  (this, last)

takeF :: Integer -> DList a -> [a]
takeF 0     _                 = []
takeF (n+1) (DLNode _ x next) = x : (takeF n next)

takeR :: Show a => Integer -> DList a -> [a]
takeR 0     _                 = []
takeR (n+1) (DLNode prev x _) = x : (takeR n prev)
