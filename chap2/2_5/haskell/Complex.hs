module Complex where

data CN a = C a a
    deriving (Eq, Show)

instance (Num a) => Num (CN a) where
    (C x y) + (C z w) = C (x + z) (y + w)
    negate (C x y) = C (-x) (-y)
    (C x y) * (C z w) = C (x*z - y*w) (x*w + y*z)
    abs (C x y) = C (x*x+y*y) 0
    fromInteger a = C (fromInteger a) 0

instance (Fractional a) => Fractional (CN a) where
    (C x y) / (C z w) = C ((x*z + y*w) / (z*z - w*w)) ((y*z - x*w) / (z*z - w*w))


fromRI :: (Num a) => a -> a -> CN a
fromRI = C

-- fromMA :: (Num a) => a -> a -> C

real :: (Num a) => CN a -> a
real (C x y) = x
imag :: (Num a) => CN a -> a
imag (C x y) = y

magnitude :: (Num a, Floating a) => CN a -> a
magnitude (C a b) = sqrt ((sqr a) + (sqr b))
    where sqr x = x * x

project :: (Eq a, Num a) => CN a -> Either a (CN a)
project (C x 0) = Left x
project (C x y) = Right (C x y)

