module Rational where

import           Data.Ratio (denominator, numerator)

data Rat = Rat Integer Integer
    deriving (Eq, Show)

instance Num Rat where
    (Rat a b) + (Rat c d) = makeRat (a*d + c*b) (b * d)
    negate (Rat a b) = makeRat (-a) b
    (Rat a b) * (Rat c d) = makeRat (a * c) (b * d)
    fromInteger a =  Rat (fromInteger a) 1

instance Fractional Rat where
    (Rat a b) / (Rat c d) = makeRat (a * d) (b * c)
    fromRational x = Rat n d
        where n = numerator (toRational x)
              d = denominator (toRational x)


toRat :: Integer -> Rat
toRat a = Rat a 1

makeRat :: Integer -> Integer -> Rat
makeRat n 0 = error "ERROR:Denominator is 0"
makeRat n d = Rat (div n g) (div d g)
    where g = gcd n d

numer :: Rat -> Integer
numer (Rat n d) = n

denom :: Rat -> Integer
denom (Rat n d) = d

raise :: Rat -> Double
raise (Rat n d) = (realToFrac n) / (realToFrac d)

project :: Rat -> Integer
project (Rat n d)
    | d == 1 = n
    |otherwise = error "ERROR: unprojectable" (Rat n d)

