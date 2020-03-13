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

data Comparison = LessThan | EqualTo | GreaterThan

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
  compated _ _ = EqualTo

instance Order a => Order (List a) where
  compared Nil Nil = EqualTo
  compared (Cons h1 t1) (Cons h2 t2) = case compared h1 h2 of
    EqualTo -> 

comparedEqual :: Order x => x -> x -> Bool
comparedEqual =
  error "TODO: comparedEqual"

lessThanOrEqual :: Order x => x -> x -> Bool
lessThanOrEqual =
  error "TODO: lessThanOrEqual"

isSorted :: Equal x => List x -> Bool
isSorted =
  error "TODO: isSorted"

-- 2.3 Exercises from the lecture

mapList :: (a -> b) -> List a -> List b
mapList =
  error "TODO: mapList"

flipList ::
  List (a -> b) -> a -> List b
flipList list a =
  mapList (\func -> func a) list

mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional =
  error "TODO: mapOptional"

flipOptional ::
  Optional (a -> b) -> a -> Optional b
flipOptional opt a =
  mapOptional (\func -> func a) opt

data ParseResult x
  = ParseError String
  | ParseSuccess x String
  deriving Show

data Parser x
  = Parser (String -> ParseResult x)

runParser :: Parser a -> String -> ParseResult a
runParser (Parser f) =
  f

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
mapParser =
  error "TODO: mapParser"

flipParser ::
  Parser (a -> b) -> a -> Parser b
flipParser prsr a =
  mapParser (\func -> func a) prsr

data Vector6 a
  = Vector6 a a a a a a

mapVector6 :: (a -> b) -> Vector6 a -> Vector6 b
mapVector6 =
  error "TODO: mapVector6"

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
