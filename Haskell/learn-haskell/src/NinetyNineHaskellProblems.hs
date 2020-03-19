module NinetyNineHaskellProblems where

import Control.Exception
import Formatting
import Formatting.Clock
import Text.Printf
import System.Clock

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

-- Mock implementation of foldl' to illustrate the principle of forced
-- eager evaluation by means of seq function
foldl_eager :: (b -> a -> b) -> b -> [a] -> b
foldl_eager fn val [] = val
foldl_eager fn val (x:xs) =
  let z = val `fn` x
  in seq z $ foldl_eager fn z xs

-- (Very) basic benchmarking function designed to print the execution time of an action
benchmark :: a -> IO()
benchmark a =
  do
    start <- getTime Monotonic
    evaluate a
    end <- getTime Monotonic
    printf "%.2f \n" (toMs end start)
  where
    toMs t2 t1 = 1e-6 * val
      where val = fromIntegral $ toNanoSecs (diffTimeSpec t2 t1) :: Float
