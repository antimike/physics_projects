
{-
Implementation of standard 'while' and 'for' control structures in Haskell.
Unsurprisingly, these functions are both monadic (to handle side effects) and
recursive (to handle repetition).
The implementations and exmaples are taken from <http://www.haskellforall.com/2012/01/haskell-for-c-programmers-for-loops.html>.
-}
module Loop where

  import Control.Monad

  while :: Monad m => m Bool -> m a -> m ()
  while cond act = do
    curr <- cond
    when curr $ do
      act
      while cond act

  for :: Monad m => m a -> m Bool -> m b -> m c -> m ()
  for first cond post act = do
    first
    while cond $ do
      act
      post
