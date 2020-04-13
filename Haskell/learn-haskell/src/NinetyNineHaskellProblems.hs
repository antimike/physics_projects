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
import Data.List
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

-- Type defns used in this problem:
-- 'Flagged' pairs a type with a boolean flag.  Useful for indicating whether
-- a 'carry' operation needs to be propagated after 'rectifying' a pair of chars.
-- 'Bookends' is a type synonym for a pair of chars.  The name indicates their origin:
-- the first and last chars (i.e., 'bookends') of a string are recursively 'peeled'
-- off of a string into a list.
newtype Flagged a = Flag (Bool, a)
newtype Bookends = Bookend (Char, Char)
type BookendsOp = Bookends -> Bookends
instance Show (Bookends) where
  show b = "(" ++ (show $ front b) ++ ", " ++ (show $ back b) ++ ")"
instance Eq (Bookends) where
  (==) b1 b2 = front b1 == front b2 && back b1 == back b2
instance Ord (Bookends) where
  (<=) b1 b2
    | front b1 == front b2  = back b1 < back b2
    | otherwise             = front b1 < front b2
instance Enum (Bookends) where
  toEnum n    = Bookend (decodeInt n) where
    decodeInt = chr.(flip div $ 128) &&& chr.(flip mod $ 128)
  fromEnum b  = (uncurry (+)) (radixify b) where
    radixify  = (* 128).ord.front &&& ord.back
instance Functor (Flagged) where
  fmap fn (Flag (b, x)) = Flag (b, fn x)
instance Applicative (Flagged) where
  pure x = Flag (False, x)
  (<*>) (Flag (b, fn)) (Flag (b', x)) = Flag (b || b', fn x)

front :: Bookends -> Char
back :: Bookends -> Char
flag :: Bookends -> Flagged Bookends
front (Bookend (c1, c2)) = c1
back (Bookend (c1, c2)) = c2
flag b = Flag (back b > front b, b)

-- Implementation of 'peel' function and helpers 'middle' and 'peel`'
-- 'peel' returns a list of (char, char) pairs by 'peeling off' the first and last
-- chars in a string until none are left.
-- The helper defns. are pointful despite their simplicity because the base cases
-- involve error handling that can't (?) be written in a point-free way.
-- peel' :: String -> [PairOf Char]
-- peel' [] = []
-- peel' [x] = [link x sentinel]
-- peel' str = peel str
-- middle :: [a] -> [a]
-- middle [] = []
-- middle [x] = [x]
-- middle xs = init.tail $ xs
-- peel = (uncurry (:)) . (Bookend.(head &&& last) &&& (peel'.middle))

peel :: String -> [PairOf Char]
peel [] = []
peel [x] = [link x sentinel]
peel (x:xs) = link x (last xs) : (peel $ init xs)

-- Miscellaneous stuff--might be useful, might not
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)
sentinel = chr 0
isNum :: Char -> Bool
isNum = (uncurry (&&)).((<= '9') &&& (>= '0'))
isWhitespace :: Char -> Bool
isWhitespace c = c == ' '


nextOp :: PairOf Char -> Transformation PairOf Char Char
nextOp p = link firstOf (carry.secOf) where
  carry = if firstOf p < secOf p && firstOf p /= sentinel then succ else id

nextOp' :: PairOf Char -> Transformation PairOf Char Char

step1 :: [PairOf Char] -> (Transformation PairOf Char Char, [PairOf Char])
step1 ends = mapAccumL  (\trans pair -> nextOp &&& ((|>) $ duplicate firstOf)
  $ (trans |> pair))
  ident ends

step2 ::
  (Transformation PairOf Char Char, [PairOf Char]) ->
  (Transformation PairOf Char Char, [PairOf Char])
step2 (trans, ends) = mapAccumL (\trans pair -> )

-- Note that this generalizes the notion of a linear transformation
-- This is equivalent to x |> y = extend (extract x $ y), where extract / extend
-- are the standard comonad functions.  TODO: Rewrite as a subclass / specialization of
-- comonad?
-- The idea in the context of this problem is to "transform" a pair via a pair
-- of functions of type (Char, Char) -> Char
class Transformable t where
  (|>) :: t (t a -> b) -> t a -> t b
type Transformation t a b = t (t a -> b)

type CharFn = Char -> Char
newtype PairOf a = PairWith {applicator :: forall b. (a -> a -> b) -> b}
instance (Show a) => Show (PairOf a) where
  show (PairWith f) = f $ \x y -> "(" ++ show x ++ ", " ++ show y ++ ")"
instance Functor (PairOf) where
  fmap fn (PairWith g) = PairWith $ \h -> h (fn.g $ \x _ -> x) (fn.g $ \_ y -> y)
instance Applicative (PairOf) where
  pure x = PairWith $ \f -> f x x
  (<*>) fns args = PairWith $ \fn -> fn (firstOf fns $ firstOf args) (secOf fns $ secOf args)
instance Transformable (PairOf) where
  (|>) fns args = PairWith $ \fn -> fn (firstOf fns $ args) (secOf fns $ args)

link :: a -> a -> PairOf a
link x y = PairWith $ \f -> f x y

firstOf :: PairOf a -> a
secOf :: PairOf a -> a
switch :: Transformation PairOf a a
ident :: Transformation PairOf a a
duplicate :: (PairOf a -> b) -> Transformation PairOf a b
firstOf p = (applicator p) $ (\x _ -> x)
secOf p = (applicator p) $ (\_ y -> y)
switch = link secOf firstOf
ident = link firstOf secOf
duplicate fn = link fn fn


-- incBookends :: Bookends -> Bookends
-- incBookends = fst &&& (incNonWhitespace.snd)
--   where incNonWhitespace c = if (isWhitespace c) c else inc c

-- rectifySnd :: Bookends -> Flagged Bookends
-- rectifySnd (x, y) = (not (isSpace y) && y > x, (x, x))

-- flagPair :: (Char, Char) -> Flagged (Char, Char)
-- flagPair (x, y) = (not (isSpace y) && y > x, (x, x))

-- TODO: Rewrite using (type-parameterized?) Monads / Arrows
-- propagateFlag :: (Bookends -> Bookends) -> Flagged Bookends -> Flagged Bookends
-- accr :: Flagged [Bookends] -> Flagged Bookends -> Flagged [Bookends]

-- foldl (\acc next -> (fst acc ++ pure.rect.(snd acc) $ next, )) ([], id) peeled

{-
Options:
1. Functor / monad / applicative instance for 'Flagged' type
2. Arrow-based computation
3. Traversable
4.
-}


-- Beginnings of a more 'sophisticated' solution using arrow-like operators
{-
Note: The following can almost certainly be accomplished more simply using arrows.
*** NOTE: Use MapAccumL (!!!)
-}


-- class (Monoid h) => MutableOperator m a b where
--   begin :: a -> m a b
--   continue :: m a b -> a -> m a b
--   history :: m a b -> h b


  -- infixl 7

-- newtype Propagator a b = Propagator {
--   hist' :: [b],
--   comp' :: a -> b,
--   prop' :: a -> Residue a b -> Residue a b
-- }
--
-- instance MutableOperator (Propagator a b) where
--   process = prop'



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
