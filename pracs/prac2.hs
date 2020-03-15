-- 2.1 Equal

class Equal a where
  isEqual :: a -> a -> Bool

instance Equal Bool where
  isEqual False False = True
  isEqual True True   = True
  isEqual _ _         = False

data Optional a
  = Empty
  | Full a
  deriving Show

instance Equal a => Equal (Optional a) where
  isEqual Empty Empty = True
  isEqual (Full a) (Full b) = isEqual a b
  isEqual _ _ = False

data List a
  = Nil
  | Cons a (List a)
  deriving Show

instance Equal a => Equal (List a) where
  isEqual Nil Nil = True
  isEqual (Cons h1 t1) (Cons h2 t2) = (isEqual h1 h2) && (isEqual t1 t2)
  isEqual _ _ = False

lookupByKey :: Equal key => key -> List (key, value) -> Optional value
lookupByKey key Nil = Empty
lookupByKey key (Cons (k, v) t) = if isEqual key k then Full v else lookupByKey key t

--- 2.2 Order

data Comparison = LessThan | EqualTo | GreaterThan deriving Show

instance Equal Comparison where
  isEqual LessThan LessThan = True
  isEqual EqualTo EqualTo   = True
  isEqual GreaterThan GreaterThan   = True
  isEqual _ _   = False


class Equal x => Order x where
  compared :: x -> x -> Comparison

instance Order Bool where
  compared True False = GreaterThan
  compared False True  = LessThan
  compared _ _ = EqualTo

instance Order a => Order (List a) where
  compared Nil Nil = EqualTo
  compared (Cons h1 t1) (Cons h2 t2) = case compared h1 h2 of
    EqualTo -> compared t1 t2
    GreaterThan -> GreaterThan
    LessThan -> LessThan
  compared _ Nil = GreaterThan
  compared Nil _ = LessThan


comparedEqual :: Order x => x -> x -> Bool
comparedEqual a b = isEqual a b

lessThanOrEqual :: Order x => x -> x -> Bool
lessThanOrEqual a b = case compared a b of
  EqualTo -> True
  LessThan -> True
  GreaterThan -> False

isSorted :: Order x => List x -> Bool
isSorted Nil = True
isSorted (Cons _ Nil) = True
isSorted (Cons h (Cons h1 t)) = lessThanOrEqual h1 h  && isSorted (Cons h1 t)


-- 2.3 Exercises from the lecture

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons h t) = Cons (f h) (mapList f t)

flipList :: List (a -> b) -> a -> List b
flipList list a = mapList (\func -> func a) list

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty = Empty
mapOptional f (Full a) = Full (f a)

flipOptional :: Optional (a -> b) -> a -> Optional b
flipOptional opt a = mapOptional (\func -> func a) opt

data ParseResult x
  = ParseError String
  | ParseSuccess x String
  deriving Show

data Parser x
  = Parser (String -> ParseResult x)

runParser :: Parser a -> String -> ParseResult a
runParser (Parser f) = f

charParser :: Char -> Parser Char
charParser c =
  Parser (\s ->
      case s of
        (x:xs) ->
          if x == c
          then ParseSuccess x xs
          else ParseError (show x ++ " is not " ++ show c)
        [] ->
          ParseError "unexpected end of input"
    )

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser = error "todo"
--mapParser f (Parser a) = Parser (f a)

flipParser ::
  Parser (a -> b) -> a -> Parser b
flipParser prsr a =
  mapParser (\func -> func a) prsr

data Vector6 a
  = Vector6 a a a a a a deriving Show

mapVector6 :: (a -> b) -> Vector6 a -> Vector6 b
mapVector6 f (Vector6 a1 a2 a3 a4 a5 a6) = Vector6 (f a1) (f a2) (f a3) (f a4) (f a5) (f a6)

flipVector6 ::
  Vector6 (a -> b) -> a -> Vector6 b
flipVector6 v6 a =
  mapVector6 (\func -> func a) v6

--- 2.4 CanMap

class CanMap f where
  mapAnything :: (a -> b) -> f a -> f b

instance CanMap List where
  mapAnything = mapList

instance CanMap Optional where
  mapAnything = mapOptional

instance CanMap Parser where
  mapAnything = mapParser

instance CanMap Vector6 where
  mapAnything = mapVector6

flipAnything ::
  Functor f =>
  f (a -> b) ->
  a ->
  f b
flipAnything fa a =
  error "TODO: flipAnything"

--- My stuff

applyOptional2 :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
applyOptional2 _ Empty _ = Empty
applyOptional2 _ _ Empty = Empty
applyOptional2 f (Full a) (Full b) = Full(f a b)
