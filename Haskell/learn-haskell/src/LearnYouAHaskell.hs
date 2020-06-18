module LearnYouAHaskell where

type Alias = Int
type Pair = (Int, Int)

testFn :: Int -> Int
testFn = \x -> x + 1

incLeft :: Int -> Pair -> Maybe(Pair)
incRight :: Int -> Pair -> Maybe(Pair)

incLeft n (l, r)
  | abs(l + n - r) < 4  = Just (l + n, r)
  | otherwise           = Nothing

incRight n (l, r)
  | abs(r + n - l) < 4  = Just (l, r + n)
  | otherwise           = Nothing

example :: Maybe(Pair)
example = do
  x <- return (1, 1)
  y <- incLeft 1 x
  z <- incRight (-3) y
  w <- incLeft 4 z
  incRight 2 w

-- vector :: (Float, Float, Float)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Read, Show, Eq, Enum, Ord)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen |King
  deriving (Read, Show, Eq, Enum, Ord)

data Card = Card {
  rank :: Rank,
  suit :: Suit
} deriving (Read, Show, Eq)

instance Ord Card where
  compare card1 card2 = compare (rank card1, suit card1) (rank card2, suit card2)

instance Enum Card where
  toEnum num = let (rk, suit) = divMod num 4 in Card (toEnum rk) (toEnum suit)
  fromEnum cd = fromEnum(suit cd) * 4 + fromEnum(rank cd)

type Deck = [Card]

deck :: Deck
deck = [Card r s | r <- [Ace .. King], s <- [Clubs .. Spades]]

data UnitVector = UnitVector {
  polarAngle :: Float,
  azimuth :: Float
}



data BinaryTree a = EmptyTree | Root a (BinaryTree a) (BinaryTree a)
  deriving (Read, Show, Ord, Eq)

instance Functor BinaryTree where
  fmap fn EmptyTree = EmptyTree
  fmap fn (Root val left right) = Root (fn val) (fmap fn left) (fmap fn right)

leaf :: (Ord a, Show a, Read a, Eq a) => a -> BinaryTree a
leaf x = Root x EmptyTree EmptyTree

addNode :: (Ord a, Show a, Read a, Eq a) => a -> BinaryTree a -> BinaryTree a
addNode val tree@(Root r left right)
  | val < r     = Root r (addNode val left) right
  | val > r     = Root r left (addNode val right)
  | otherwise   = tree

searchTree :: (Ord a, Show a, Read a, Eq a) => BinaryTree a -> a -> Bool
searchTree EmptyTree _ = False
searchTree (Root root left right) val
  | root < val   = searchTree right val
  | root > val   = searchTree left val
  | otherwise    = True

-- TODO: Implement
-- rebaseTree :: (Ord a, Show a, Read a, Eq a) => BinaryTree a -> a -> BinaryTree a

-- Contexts∷
-- 1. Experiments / Measurements
-- 2. Units
-- 3. Errors
-- 4. Spin (?)
-- 5. Quantum State
{-
How does a function which produces a quantum state process a "classical" state?
By contrast, how does it process another quantum state?
QState = [state1, state2, state3]
f(state) = states
==> f(states) = [f(state) | state <- states]
ensemble of quantum states: "nested" monad
Superposition :: Monad
- How to guarantee that amplitudes sum to 1 programmatically?
  - "N-Branching" of lists of numbers
  {x, y, z} -> {x/2, x/2, y, z}
- Matrix exponentials
- Desired behavior:
  Given a measurement setup and an initial state, calculate or approximate some
  relevant quantity.
- Measurement apparatus has well-defined behavior on classical states
- Reversible vs. probabilistic measurements
- qstate >>= measurement
- Hermitian conjugate (adjoint)
- class Hilbert (?)
  - derives from Monoid:
    - mzero = vacuum
    - mplus = superpose
    - fmap = lift
    - bind = action on basis vectors
    - Implements additional structure: inner product
      - Functor?

* Question: Given a "free" context constructor, how do we represent its operation on
  contextual objects?
- Context constructors in physics:
  - Ensemble
    --> Meta-ensembles?
  - State
  - Coordinates
  - Tensors?
    --> Bind == covariant derivative?
    ...or Bind == "tensor of tensor"
  - Differential forms
    --> Bind == exterior derivative from del
  - Operator
    --> Bind == commutator; composition
  - Category: Operators on H
    - Left / right identities: trivial
    - Not associative:
      [A, [B, C]] + [B, [C, A]] + [C, [A, B]] = 0
  - Map x -> operator: quantum field
    - Composition of fields: tensor product of Fock spaces
-}

-- Ten commandments of grad school:
-- When you're at work:
-- 1. Don't play chess.
-- 2. Don't sleep.
-- 3. Don't do work in common spaces.
-- 4. Reply to emails right away.
-- 5. Don't read blogs or have non-math / physics tabs open.
-- 6. Don't read Quora.
-- 7. Make a point of talking to and interacting with people--start conversations about physics.
-- 8. Collaborate on work.
-- 9. Dress nicely.
-- 10. Don't talk about outside life with colleagues.
--
-- Grad school hadith:
-- 1. The best apology consists of impressive work.
