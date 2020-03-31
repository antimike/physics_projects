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

-- For SPOJ-y problems involving long input strings / char arithmetic
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import qualified Data.ByteString.Char8 as BSC

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

{-
Polyvariadic functions:
Approach is to define a "wrapper" function of type [a] -> r that uses
continuation passing and pattern matching to infer the correct instance type
from its context
-}


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
newtype PairT a b = PairT { unpack :: forall c. (a -> b -> c) -> c }

instance (Show a, Show b) => Show (PairT a b) where
  show (PairT up) = up $ \x y -> "(" ++ show x ++ ", " ++ show y ++ ")"

pairT a b = PairT $ (\f -> f a b)
fstT :: PairT a b -> a
fstT (PairT up) = up $ \x _ -> x
sndT :: PairT a b -> b
sndT (PairT up) = up $ \_ y -> y

-- Ideas: Haar transform
  -- > Breakpoint detector for large datasets

-- SPOJ problem: next palindrome
-- Prerequisites (also useful for other SPOJ problems):
  -- String arithmetic (for very long integer inputs)

{-
How to build the next palindrome:

- reverse input
- x:[] :-> x
- x:y:[] :-> (x:y)
- f m l :->
  - l m l if l >= f
  - f (m + 1) f otherwise

8999999999

899  :->  909

- List of "endpoints" (i.e., (f:m:l) -> (f, l) applied recursively)
- Recursive datatype to encapsulate parsing logic?

y = reverse x - x
z = positives y
s = max x reverse x



-}

inc :: Char -> Char
inc = chr . (+ 1) . ord


peel' :: String -> [(Char, Char)]
peel' [] = []
peel' [x] = [(x, chr 0)]
peel' str = peel str
middle :: [a] -> [a]
middle [] = []
middle [x] = [x]
middle xs = init.tail $ xs
peel = (uncurry (:)) . ((head &&& last) &&& (peel'.middle))

propagate :: EndsWithCarry -> EndsWithCarry

isNum :: Char -> Bool
isNum = (uncurry (&&)).((<= '9') &&& (>= 0))

applyCarry :: Bool -> (Char, Char) -> (Char, Char)
applyCarry b (x, y)
  | b && isnum y   = (x, inc y)
  | otherwise      = (x, y)
--
extractCarry :: (Char, Char) -> Bool
extractCarry (x, y) = isnum y && y > x

type PeeledWithCarry = (Bool, [(Char, Char)])

accumulator :: Bookends -> Flagged PeeledString -> Flagged PeeledString
accumulator (x, y) r =

-- rectify = fst &&& (uncurry max)

type Flagged a = (Bool, a)
type PeeledString = [(Char, Char)]
type Bookends = (Char, Char)

isWhitespace :: Char -> Bool
isWhitespace c = c == ' '

incBookends :: Bookends -> Bookends
incBookends = fst &&& (incNonWhitespace.snd)
  where incNonWhitespace c = if (isWhitespace c) c else inc c

rectifySnd :: Bookends -> Flagged Bookends
rectifySnd (x, y) = (not (isSpace y) && y > x, (x, x))

-- flagPair :: (Char, Char) -> Flagged (Char, Char)
-- flagPair (x, y) = (not (isSpace y) && y > x, (x, x))

applyFlag :: Flagged Bookends -> Flagged Bookends
applyFlag (f, b) =

rectify :: (Bookends -> Char) -> Bookends -> Bookends
rectify fn = fn &&& fn

-- TODO: Rewrite using (type-parameterized?) Monads / Arrows
propagateFlag :: (Bookends -> Bookends) -> Flagged Bookends -> Flagged Bookends

lessThan :: Bookends -> Bookends -> Bool
lessThan b1 b2
  | fst b1 == fst b2  = snd b1 < snd b2
  | otherwise         = fst b1 < fst b2

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

a -> ((a -> a), [a]) -> ((a -> a), [a])

{-
Note: The following can almost certainly be accomplished more simply using arrows.
*** NOTE: Use MapAccumL (!!!)
-}


class (Monoid h) => MutableOperator m a b where
  begin :: a -> m a b
  continue :: m a b -> a -> m a b
  history :: m a b -> h b
  mapo ::


  infixl 7

newtype Propagator a b = Propagator {
  hist' :: [b],
  comp' :: a -> b,
  prop' :: a -> Residue a b -> Residue a b
}

instance MutableOperator (Propagator a b) where
  process = prop'



{-
2002
String -> list of bookends
Apply accumulator: plus one / plus ten

folder: pre op post

"Operator" class: Takes a value and a function, returning a value and a function
-}
-- dropwhile isspace : trim spaces



-- 88889999 -> [89, 89, 89, 89] -> [(99, T), (99, T), (99, T), (99, T)]
-- ->
-- 888999     | 888999     | 888999     | 888999
-- 999888 (F) | 899888 (T) | 889888 (T) | 888888 (T)

{-
- Transcendental recursive data structures?
- Generating fns of data structures?
(Inspired by reading about data structure derivatives)
-}
