{-# OPTIONS_GHC -Wall #-}

{- IMPORTS -}

import Data.Char

import Data.Functor
import Control.Applicative hiding ((<|>), some, many)

import Test.Framework


{- TYPES, CLASSES AND INSTANCES -}


data List a = Nil | Cons a (List a)
  deriving (Eq, Show)


data Pair a b = Pair a b
  deriving (Eq, Show)


data NonEmptyList a = NonEmptyList a (List a)
  deriving (Eq, Show)


data Operation =
   Add
   | Subtract
   | Divide
   | Multiply
   deriving (Eq, Show)


instance Ord Operation where
   compare Multiply Add = GT
   compare Multiply Subtract = GT
   compare Divide Add = GT
   compare Divide Subtract = GT
   compare Add Multiply = LT
   compare Add Divide = LT
   compare Subtract Multiply = LT
   compare Subtract Divide = LT
   compare _ _ = EQ


data Expression =
   Number Int
   | Op Expression Operation Expression
   | Parens Expression
   deriving (Eq, Show)


instance Arbitrary Expression where
   arbitrary = genExpression


data ParseResult x =
   ParseError String
   | ParseSuccess x String
   deriving (Eq, Show)


instance Functor ParseResult where
   fmap f pr = case pr of
      ParseError err -> ParseError err
      ParseSuccess x str -> ParseSuccess (f x) str


data Parser x = Parser (String -> ParseResult x)


instance Functor Parser where
   fmap f (Parser p) = Parser (\s -> fmap f (p s))


instance Applicative Parser where
   pure a = Parser (\s -> ParseSuccess a s)

   (<*>) (Parser f) (Parser a) = Parser (\s -> case f s of
      ParseError err -> ParseError err
      ParseSuccess f' s' -> case a s' of
         ParseError err -> ParseError err
         ParseSuccess a' s'' -> ParseSuccess (f' a') s'')


instance Monad Parser where
   return a = Parser (\s -> ParseSuccess a s)

   (>>=) (Parser a) f = Parser (\s ->
      case a s of
         ParseError err -> ParseError err
         ParseSuccess x s' -> let (Parser b) = (f x) in b s')


{- HELPER FUNCTIONS -}

-- The following 12 functions modifyed from:
-- https://gitlab.com/comp3400/pracs-outcome/-/blob/master/20200403/0400-0600Z/Prac4.hs

-- Runs a parser
runParser :: Parser a -> String -> ParseResult a
runParser (Parser f) = f


-- FoldRight for the (List a) type
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h `Cons` t) = f h (foldRight f b t)


-- FoldLeft for the (List a) type
foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h `Cons` t) = let b' = f b h in b' `seq` foldLeft f b' t


-- Succeeds if string is non-empty and next Char satisfies
-- the predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser
  (\s -> case s of
    (c:rest) -> if predicate c then ParseSuccess c rest else
                  ParseError ("Parse error: unexpected '" ++ [c] ++ "'")
    _ -> ParseError "Parse error: unexpected end of input")


-- Parse a specific character
char :: Char -> Parser Char
char c = satisfy (== c)


-- If the first parser fails, try the second parser
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser (\s -> case runParser p1 s of
  success@(ParseSuccess _ _) -> success
  ParseError _err -> runParser p2 s
  )


-- Run the parser as many times as possible;
-- must succeed at least once;
-- collect non-empty list of results.
some :: Parser a -> Parser (NonEmptyList a)
some p = NonEmptyList <$> p <*> many p


-- Run the parser as many times as possible;
-- collect list of results.
many :: Parser a -> Parser (List a)
many p = liftA2 Cons p (many p) <|> pure Nil


-- Parse zero or more values separated by something
sepBy :: Parser a -> Parser sep -> Parser (List a)
sepBy element sep =
  liftA2 Cons element (many (sep *> element))
  <|> pure Nil


-- Parse a digit ('0' .. '9')
parseDigit :: Parser Int
parseDigit = c2i <$> satisfy (\c -> c >= '0' && c <= '9')
  where
  c2i c = fromIntegral (ord c - ord '0')


-- Parse non-signed int
parseInt :: Parser Int
parseInt = f <$> some parseDigit
  where
  f (NonEmptyList x xs) = foldLeft (\n -> (n * 10 +)) 0 (x `Cons` xs)


-- Parse a signed int
parseSignedInt :: Parser Int
parseSignedInt =
  ((char '-' $> ((-1) *)) <*> parseInt) <|> parseInt


-- Handles the trailing string after expression.
-- If the trailing string contains a non-space
-- character, we want to report that over the
-- whitespace, as it is more useful, but if there
-- is no non-space characters then report whitespace
-- as unexpected
endStringParser :: Parser ()
endStringParser = Parser handleEndString


-- Helper function for endStringParser
handleEndString :: String -> ParseResult ()
handleEndString (' ':[]) = ParseError "Parse error: unexpected ' '"
handleEndString (' ':s)  = handleEndString s
handleEndString (x:_)    = ParseError ("Parse error: unexpected '" ++ [x] ++ "'")
handleEndString []       = ParseSuccess () ""


-- Given a function and a pair, apply the arguments of pair to function
takePair :: (a -> b -> c) -> ((Pair a b) -> c)
takePair f = \(Pair a b) -> f a b


{- API FUNCTIONS -}


-- Reassociate the syntax tree according to order of precedence
-- e.g. Op (Op (Number 3) Add (Number 5)) Multiply (Number 8)
--   to Op (Number 3) Add (Op (Number 5) Multiply (Number 8))
reassociate :: Expression -> Expression
reassociate (Number n) = Number n
reassociate (Parens expr) = Parens (reassociate expr)
-- If operators are equal, we still want to associate left
reassociate (Op expr1 op1 (Op expr2 op2 expr3)) = if op1 >= op2
   then (Op (Op (reassociate expr1) op1 (reassociate expr2)) op2 (reassociate expr3))
   else (Op (reassociate expr1) op1 (Op (reassociate expr2) op2 (reassociate expr3)))
reassociate (Op (Op expr1 op1 expr2) op2 expr3) = if op1 >= op2
   then (Op (Op (reassociate expr1) op1 (reassociate expr2)) op2 (reassociate expr3))
   else (Op (reassociate expr1) op1 (Op (reassociate expr2) op2 (reassociate expr3)))
reassociate (Op expr1 op expr2) = Op (reassociate expr1) op (reassociate expr2)


{--
   Changed the grammar slightly to avoid infinite left recursion
   Still has same functionality

   Term        -> N [Operator N]
   N           -> Number | '(' Term ')'
   Operator    -> Whitespace Operator' Whitespace
   Operator'   -> '+' | '-' | '*' | '/'
   Number      -> ['-'] Digit+
   Digit       -> '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
   Whitespace  -> ' '+

--}


{-- 
  Some associativity errors.
  >>> 2 + 3 * 4 - 5 * 6 + 7
  97
  >>> 2 + 3 * 4 - 5 / 6 + 7
  9
  >>> (2 + 3) * 4 - 5 * 6 + 7
  97
--}
-- Converts a string into an Expression, or Error
parseExpression :: Parser Expression
parseExpression = (reassociate . takePair leftAssociate) <$> liftA2 Pair parseN (many (liftA2 Pair parseOperator parseN))


-- Given an expression and a list of operator - expression pairs, create a
-- new expression with sub expressions left associated
-- e.g. leftAssociate 6 [(+,7) (-,8) (* 9)] = Op (Op (Op 6 + 7) - 8) * 9
-- Note it doesn't factor in operator predicence, that is done by the reassociate function
leftAssociate :: Expression -> List (Pair Operation Expression) -> Expression
leftAssociate expr Nil = expr
leftAssociate expr1 (Cons (Pair op expr2) list) = leftAssociate (Op expr1 op expr2) list


-- Parser for an Operation
-- Note: changeing 'some' to 'many' here would allow 0 or some
-- instances of whitespace, which I think is better, but the spec
-- specifies one or more.
parseOperator :: Parser Operation
parseOperator = some (char ' ') *> Parser (\s -> case s of
   ('+':rest) -> ParseSuccess Add rest
   ('-':rest) -> ParseSuccess Subtract rest
   ('*':rest) -> ParseSuccess Multiply rest
   ('/':rest) -> ParseSuccess Divide rest
   (x:_)      -> ParseError ("Parse error: unexpected '" ++ [x] ++ "'")
   _          -> ParseError "Parse error: unexpected end of input") <* some (char ' ')


-- Parse either a number or '(' expression ')'
-- See comment on grammar above
parseN :: Parser Expression
parseN = (Number <$> parseSignedInt)
   <|> (Parens <$> (char '(' *> parseExpression <* char ')'))


-- Removes very simple expressions
simplify :: Expression -> Expression
simplify (Op expr Add (Number 0))      = simplify expr
simplify (Op (Number 0) Add expr)      = simplify expr
simplify (Op expr Subtract (Number 0)) = simplify expr
simplify (Op (Number 0) Multiply _)    = Number 0
simplify (Op _ Multiply (Number 0))    = Number 0
simplify (Op expr Multiply (Number 1)) = simplify expr
simplify (Op (Number 1) Multiply expr) = simplify expr
-- Division by 0 is allowed to return anything so we are ok in this case
simplify (Op (Number 0) Divide _)      = Number 0
simplify expr                          = expr


-- Takes an expression and reduces it to an answer
calculate :: Expression -> Int
calculate (Number n) = n
calculate (Parens expr) = calculate expr
calculate (Op expr1 op expr2) = case op of
   Add      -> value1 + value2
   Subtract -> value1 - value2
   Divide   -> value1 `div` value2
   Multiply -> value1 * value2
   where value1 = calculate expr1
         value2 = calculate expr2


-- Runs the calculator interactively
runCalculator :: IO ()
runCalculator = do
   putStr ">>> "
   line <- getLine
   if line == "q"
      then return ()
      else do
         handleLine line
         runCalculator


-- Handles a single line of input
handleLine :: String -> IO ()
handleLine line  = case parsed of
   ParseError err -> putStrLn err
   ParseSuccess n _ -> putStrLn . show $ n
   where parsed = calculate . simplify <$> (runParser (parseExpression <* endStringParser) line)


{- TESTING -}

-- Feedback: "better to use Gen Applicative interface and 'frequence' and 'sized' combinators"

-- Import the test framework introduced during the lectures. Create a
-- genExpression :: Gen Expression to execute the property tests.
genExpression :: Gen Expression
genExpression = Gen (\size gen -> reassociate $
      let (int, newGen) = next gen in
         if (int `mod` 4 == 0) then
            Parens (generate size newGen genExpression)
         else
            if size == 1 then
               Number . mod int $ 100
            else let (newGen1, newGen2) = split newGen in
               -- Here we are limiting the max 'size' of the expression to 5,
               -- otherwise this recursize call gets really expensive and running
               -- the tests will cause the cpu to spin.
               Op (generate ((min 5 size) - 1) newGen1 genExpression)
                  (generate size gen genOperation)
                  (generate ((min 5 size) - 1) newGen2 genExpression)
      )


-- Generates an Operation
-- I originally had int `mod` 4 but wsa getting an unsually large
-- number of 0's for the results, so changed to 5.
genOperation :: Gen Operation
genOperation = Gen (\_ gen -> let (int, _) = next gen in
      case int `mod` 5 of
         0 -> Divide
         1 -> Subtract
         2 -> Multiply
         _ -> Add) -- See Above


testAddRightIdentity :: Expression -> Bool
testAddRightIdentity expr = simplify (Op expr Add (Number 0)) == simplify expr

testAddLeftIdentity :: Expression -> Bool
testAddLeftIdentity expr = simplify (Op (Number 0) Add expr) == simplify expr

testSubtractRightIdentity :: Expression -> Bool
testSubtractRightIdentity expr = simplify (Op expr Subtract (Number 0)) == simplify expr

testMultiplyLeftZero :: Expression -> Bool
testMultiplyLeftZero expr = simplify (Op (Number 0) Multiply expr) == Number 0

testMultiplyRightZero :: Expression -> Bool
testMultiplyRightZero expr = simplify (Op expr Multiply (Number 0)) == Number 0

testMultiplyRightIdentity :: Expression -> Bool
testMultiplyRightIdentity expr = simplify (Op expr Multiply (Number 1)) == simplify expr

testMultiplyLeftIdentity :: Expression -> Bool
testMultiplyLeftIdentity expr = simplify (Op (Number 1) Multiply expr) == simplify expr

testZeroNumerator :: Expression -> Bool
testZeroNumerator expr = simplify (Op (Number 0) Divide expr) == Number 0


-- Runs the property tests for simplify
runTests :: IO ()
runTests = do
   test (testProperty "Add right identity" testAddRightIdentity)
   test (testProperty "Add left identity" testAddLeftIdentity)
   test (testProperty "Subtract right identity" testSubtractRightIdentity)
   test (testProperty "Multiply left zero" testMultiplyLeftZero)
   test (testProperty "Multiply right zero" testMultiplyRightZero)
   test (testProperty "Multiply right identity" testMultiplyRightIdentity)
   test (testProperty "Multiply left identity" testMultiplyLeftIdentity)
   test (testProperty "Zero Numerator" testZeroNumerator)
