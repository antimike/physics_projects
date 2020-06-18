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
-- Compared to puzzle.hs, the present solution is in the
-- continuation-passing style.

-- This code is written by Chung-chieh Shan. Nov 12, 2006.
-- It is tested with GHC and Hugs -98. See Chung-chieh Shan's post
-- on Haskell-Cafe (Nov 12, 2006) for explanations.

module Puzzle where

class Replace old new old' new' w1 w2 w3 | w1 w2 w3 -> old new old' new' where
    repl :: ((old -> new -> old) -> (old' -> new' -> old')) -> w1 -> w2 -> w3

instance Replace a a b a b a b where
    repl k = k (\old new -> new)

instance Replace old new old' new' w1 w2 w3
      => Replace [old] new old' new' Int w1 (w2 -> w3) where
    repl k i = repl (\r -> k (\old new -> case splitAt i old of
                                            (h, []   ) -> h
                                            (h, th:tt) -> h ++ r th new : tt))

-- The desired function
replace :: Replace old new old new w1 w2 w3 => w1 -> w2 -> w3
replace = repl id

-- Tests (the same as in puzzle.hs)

x1 = "abc"
x2 = ["ab", "cde", "fghi", "uvwxyz"]
x3 = [ [ [i1 + i2 + i3 | i3 <- [10..13]] | i2<- [4..5]] | i1 <- [(1::Int)..3]]

test1:: String
test1 = replace (1::Int) x1 'X'
-- "aXc"

{- The expected error reported. The error message is better, compared to that
   -- of puzzle.hs
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
