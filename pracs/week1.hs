--{-# OPTIONS_GHC -Wall #-}
x :: Integer
x = 99

f a b = (a + b) * 2

g :: (Integer -> Integer) -> Integer
g = \v -> v 88

h :: Integer
h = g (f 77)

i :: a -> b -> a
i x y = x

s = s

j = print(i 99 s)




k :: a -> a -> a
k = \a b -> a
--could also do k = k


m :: Integer -> a -> Integer
m 5 a = 6



n :: Bool -> Bool
n a = if a then False else True


data Shape =
  Rectangle Integer Integer
  | Circle Integer
  | Triangle Integer Integer Integer
  deriving (Show, Eq)

perimeter :: Shape -> Integer
perimeter = \s -> case s of
  Circle r -> r * 3 * 2
  Rectangle w h -> (w + h) * 2
  --Triangle a b c -> a + b + c

data Three a =
  Zero | One a | Two a a | Three a a a
  deriving (Eq, Show)

mapThree :: (a -> b) -> Three a -> Three b
mapThree = \f -> \g -> case g of
  Zero -> Zero
  One m -> One (f m)
  Two m m1 -> Two (f m) (f m1)
  Three m m1 m2 -> Three (f m) (f m1) (f m2)




data List a =
  Nil | Cons a (List a)
  deriving (Eq, Show)

-- headOR 33 Nil == 33
-- headOr 33 (Cons 1 (Cons 2 Nil)) == 1
heador :: a -> List a -> a
heador = \f -> _

--mapList :: (a -> b) -> List a -> List b
--mapList = 
