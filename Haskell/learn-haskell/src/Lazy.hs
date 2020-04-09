module Lazy where

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
