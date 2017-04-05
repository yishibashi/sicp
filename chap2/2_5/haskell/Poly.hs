module Poly where

data Poly a = P {
                var  :: String
              , exp  :: Double
              , coef :: a
              } deriving (Eq, Show)


instance (Num a) => Num (Poly a) where
    (P v1 e1 c1) + (P v2 e2 c2)
        |v1 == v2 && e1 == e2 = (P v1 e1 (c1+c2))
        |otherwise = (P v1 e1 c1)

