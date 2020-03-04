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

data Spinor (n :: UnitVector) => Spinor
