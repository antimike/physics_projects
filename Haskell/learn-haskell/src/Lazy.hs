module Lazy where

-- An example intended to illustrate the use of lazy pattern-matching in Haskell.
-- The function splitAt is already implemented in the Prelude but provides a convenient
-- example.
  splitAt' :: Int -> [a] -> ([a], [a])
  splitAt' n xs =
    if n <= 0
      then ([], xs)
      else
        case xs of
          [] -> ([], [])
          y:ys ->
            case splitAt' (n - 1) ys of
              ~(prefix, suffix) -> (y:prefix, suffix)
-- The ~ is used to indicate lazy pattern matching, which is necessary here to
-- avoid unnecessary overhead during recursive calls.  Specifically, the compiler does
-- not need to explicitly check that the pair constructor is actually called on every
-- level of recursion, but needs to be informed of this fact in order to act lazily (i.e.
-- optimally).
