{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE StandaloneDeriving, UndecidableInstances, FlexibleContexts #-}
-- Necessary for Scott encodings of algebraic datatypes

module NinetyNineHaskellProblems where

import Control.Exception
import Formatting
import Formatting.Clock
import Text.Printf
import System.Clock
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

-- import Data.Text.Lazy (Text)
-- import Data.Text.Lazy.Builder (Builder)
-- import Formatting

{-
Problem 1: Find the last element of a list
-}
p1_solution :: [a] -> a
p1_solution [] = error "Empty list"
p1_solution [x] = x
p1_solution (_:xs) = p1_solution xs

-- Alternative solutions illustrating useful functions / syntax
p1_solution' l = l !! (length l - 1)  -- List accessor syntax (!!)
p1_solution'' l = foldl1 (curry snd) l -- foldl1 "immediate" fold fn plus snd tuple accessor
p1_solution''' = head.reverse

{--
Problem 2: Find the penultimate member of a list
--}
-- First attempt:
p2_soln :: [a] -> Maybe a
p2_soln [] = Nothing
p2_soln [x] = Nothing
-- p2_soln l = Just $ foldl1 (curry fst) l -- Produces Just (first elem)
p2_soln [x, y] = Just(x)
p2_soln (x:xs) = p2_soln xs
-- Other solutions:
p2_soln' = last.init
p2_soln'' = head.tail.reverse

-- Pretty solution:
p2_soln''' :: Foldable f => f a -> a
p2_soln''' = fst . foldl (\(a, b) x -> (b, x)) (err1, err2)
  where err1 = error "Empty list"
        err2 = error "Singleton"

{--
Problem 3: Find the kth element of a list
--}

p3_soln :: [a] -> Int -> a
p3_soln l n = l !! (n - 1)

p3_soln' :: [a] -> Int -> Maybe a
p3_soln' l n =
  if length l >= n
    then Just $ head [snd z | z <- enumerate l, fst z >= n]
    else Nothing

-- Better implementations
p3_soln'' :: [a] -> Int -> Maybe a
p3_soln'' [] _ = Nothing
p3_soln'' (x:_) 1 = Just x
p3_soln'' (x:xs) n
  | n < 1       = error "Invalid index"
  | otherwise   = p3_soln'' xs (n - 1)

p3_soln''' l n
  | length l < n    = error "Invalid index"
  | otherwise       = fst.last $ zip l [1..n]

p3_soln'''' l n
  | length l < n    = error "Invalid index"
  | otherwise       = last $ take n l


-- "zip" already exists in the standard lib; this is merely to illustrate the principle
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys

enumerate :: [a] -> [(Int, a)]
enumerate l = zip' [1..] l

{--
Problem 4: Find the size of a list.
--}
-- p4_soln :: [a] -> Int

{--
Problem 6: Find out whether a list is a palindrome.
--}

-- For testing:
palindromize :: Eq a => [a] -> [a]
palindromize = uncurry (++) . (id &&& reverse)

big_list = [1..100000]
big_pal = palindromize big_list
big_almost = big_list ++ (drop 2 $ reverse big_list)

-- Original implementation (mine)
p6_soln, p6_soln' :: Eq a => [a] -> Bool
p6_soln [] = True
p6_soln [x] = True
p6_soln l = foldr (&&) True $ fmap comparer $ zip l $ reverse l
  where comparer x = fst x == snd x

-- Less verbose version:
p6_soln' [] = True
p6_soln' [_] = True
p6_soln' l = head l == last l && (p6_soln' . init . tail $ l)

-- Monadic version:
p6_soln_monad :: Eq a => [a] -> Bool
p6_soln_monad = liftM2 (==) id reverse

-- Sweet arrow-based implementation
-- Basically just uses == to compare l and reverse l
p6_soln_arrow :: Eq a => [a] -> Bool
p6_soln_arrow = uncurry (==) . (id &&& reverse)


-- Mock implementation of foldl' to illustrate the principle of forced
-- eager evaluation by means of seq function
foldl_eager :: (b -> a -> b) -> b -> [a] -> b
foldl_eager fn val [] = val
foldl_eager fn val (x:xs) =
  let z = val `fn` x
  in seq z $ foldl_eager fn z xs

-- (Very) basic benchmarking function designed to print the execution time of an action

{--
Approaches to improving benchmark function:
- Polyvariadic, using higher-order functions
- Using a Writer instance to consolidate string management
  - Should also use DList for efficient list concatenation
- Creating a Tester monad instance?
- Using State monad to manage computations
- Desired usage pattern:
  benchmark $ fn arg $ fn arg $ fn arg (...)
- Possibilities for automatic method detection / testing:
  - Wrapping entire module with a State monad to maintain a list of all fns / names
--}

-- varargify :: (a -> b) -> [a] -> b

-- data Problem = Problem {
--   name :: String,
--   desc :: String,
--   solns :: [a]
-- }



benchmark :: a -> IO()
benchmark a =
  do
    start <- getTime Monotonic
    evaluate a
    end <- getTime Monotonic
    printf "%.2f ms \n" (toMs end start)
  where
    toMs t2 t1 = 1e-6 * val
      where val = fromIntegral $ toNanoSecs (diffTimeSpec t2 t1) :: Float

{-
Recursive datatypes and Scott encodings
-}

-- First example: Pair implementation
newtype PairT = PairT { unpack :: forall c. (a -> b -> c) -> c }

data PairD a b (PairT t) =
  PairD { unpack :: forall c. (a -> b -> c) -> c}

-- The following doesn't work--existentials in the data constructor
-- prevent the use of direct instance declaration.
-- instance Show (PairD a b) where
--   show p = uncurry (++) (show.fstD &&& show.sndD)

deriving instance (Show a, Show b) => Show (PairD a b)

-- new_pair :: Show a Show b => a -> b -> Pair' a b
-- new_pair a b = Pair' (\f -> f a b)
fstD :: PairD a b -> a
fstD (PairD up) = up $ \x _ -> x
sndD :: PairD a b -> b
sndD (PairD up) = up $ \_ y -> y
