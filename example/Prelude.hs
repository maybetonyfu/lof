module Prelude where

type String = [Char]
undefined = error

length :: [a] -> Int
length = undefined

id :: a -> a
id = undefined



data Bool = True | False
data IO a = IO a

data Maybe a = Just a | Nothing

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>) :: m a -> m b -> m b
    (>>=) :: m a -> (a -> m b) -> m b

instance Functor Maybe
instance Functor []
instance Functor IO

instance Applicative Maybe
instance Applicative []
instance Applicative IO

instance Monad Maybe
instance Monad []
instance Monad IO

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

data Ordering = LT | EQ | GT

class Ord a where
    compare :: a -> a -> Ordering
    (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
    (>) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    max :: a -> a -> a
    min :: a -> a -> a

instance Eq Int
instance Eq Bool
instance Eq Ordering
instance Eq Float
instance Eq Char
instance Eq a => Eq [a]

instance Ord Int
instance Ord Bool
instance Ord Char

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

read :: [Char] -> a
read = undefined

show :: a -> [Char]
show = undefined

data Either a b = Left a | Right b
otherwise = True

map :: (a -> b) -> [a] -> [b]
map = undefined

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = undefined

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = undefined


head :: [a] -> a
head = undefined

tail :: [a] -> [a]
tail = undefined

zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith = undefined



fst :: (a,b) -> a
fst = undefined

snd :: (a,b) -> b
snd = undefined

(++) :: [a] -> [a] -> [a]
(++) = undefined

pi :: Float
pi = 3.14


not :: Bool -> Bool
not = undefined

class Monoid a where
  mconcat :: [a] -> a
  mappend :: a -> a -> a
  mempty :: a

instance Monoid [a]

const  :: a -> b -> a
const = undefined


reverse :: [a] -> [a]
reverse = undefined

(||) :: Bool -> Bool -> Bool
(||) = undefined

(&&) :: Bool -> Bool -> Bool
(&&) = undefined

elem :: a -> [a] -> Bool
elem = undefined

even :: Int -> Bool
even = undefined

odd  :: Int -> Bool
odd = undefined

class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a

instance Num Int
instance Num Float

sum :: Num a => [a] -> a
sum = undefined

mod :: Int -> Int -> Int
mod = undefined

class Enum a where
    enumFrom :: a -> [a]

instance Enum Int
instance Enum Char

any :: [Bool] -> Bool
any = undefined

and :: [Bool] -> Bool
and = undefined

zip :: [a] -> [b] -> [(a, b)]
zip = undefined


fromIntegral :: Num a => Int -> a
fromIntegral = undefined

(/) :: Float -> Float -> Float
(/) = undefined

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = undefined

toUpper :: Char -> Char
toUpper = undefined

toLower :: Char -> Char
toLower = undefined

sqrt :: Float -> Float
sqrt = undefined

(^) :: Num a => a -> Int -> a
(^) = undefined

floor :: Num a => a -> Int
floor = undefined

ceiling :: Num a => a -> Int
ceiling = undefined