{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- This code implements a function with the variable number of
-- arguments and the matching variable type -- replacing an element in
-- a multi-dimensional list:
--   replace :: Int -> Int -> ... -> Int -> [..[e]..] -> e -> [..[e]..]
-- where 'e' is the type of list elements, and the number of index arguments
-- (of the type Int) must match the dimensionality of the list. In other
-- words, the code implements the function that has all of these signatures
--    replace :: e -> e -> e             -- replacing in a 0-dim list
--    replace :: Int -> [e] -> e -> [e]
--    replace :: Int -> Int -> [[e]] -> e -> [[e]]
--    replace :: Int -> Int -> Int -> [[[e]]] -> e -> [[[e]]]
--    ...
-- all at the same time.
-- This problem was posed as a `Type class puzzle' by Yitzchak Gale on
-- the Haskell-Cafe mailing list in Oct 2006.

-- This code is written by Chung-chieh Shan. Nov 12, 2006.
-- It is tested with GHC and Hugs -98. See Chung-chieh Shan's post
-- on Haskell-Cafe (Nov 12, 2006) for explanations.

module Puzzle where

class Replace'' n old new where
  repl'' :: n -> old -> new -> old

instance Replace'' () a a where
  repl'' () old new = new

instance Replace'' n old new => Replace'' (n,Int) [old] new where
  repl'' (i,i0) old new =
    case splitAt i0 old of (h,[]   ) -> h
                           (h,th:tt) -> h ++ repl'' i th new : tt

class Replace' n n' old new where
  repl' :: n -> n' -> old -> new -> old

instance Replace'' n old new => Replace' n () old new where
  repl' n () = repl'' n

instance Replace' (n1,n2) n3 old new => Replace' n1 (n2,n3) old new where
  repl' n1 (n2,n3) = repl' (n1,n2) n3

class Replace n a b c where
  repl :: n -> a -> b -> c

instance Replace' () n [old] new => Replace n [old] new [old] where
  repl = repl' ()

instance Replace (i,n) a b c => Replace n i a (b->c) where
  repl i0 i = repl (i,i0)

-- The desired function, whose inferred type is
--   replace :: (Replace () a b c) => a -> b -> c
replace n = repl () n


-- Tests

x1 = "abc"
x2 = ["ab", "cde", "fghi", "uvwxyz"]
x3 = [ [ [i1 + i2 + i3 | i3 <- [10..13]] | i2<- [4..5]] | i1 <- [(1::Int)..3]]

test1:: String
test1 = replace (1::Int) x1 'X'
-- "aXc"

{- expected error reported
test2:: [String]
test2 = replace (1::Int) x2 'X'
-}

test3:: [String]
test3 = replace (1::Int) x2 "X"
-- ["ab","X","fghi","uvwxyz"]

test4:: [String]
test4 = replace (2::Int) (1::Int) x2 'X'
-- ["ab","cde","fXhi","uvwxyz"]

test5:: [[[Int]]]
test5 = replace (2::Int) (0::Int) (1::Int) x3 (100::Int)
-- [[[15,16,17,18],[16,17,18,19]],[[16,17,18,19],[17,18,19,20]],
--  [[17,100,19,20],[18,19,20,21]]]


