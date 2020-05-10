{--
This assignment contributes to 25% of the total marks for COMP3400.
• The marks for this assignment out of 25 will be distributed as follows:
– parseExpression function 7 marks
– simplify function with 2 marks
– genExpression function 3 marks
– runTests function 2 marks
– calculate function 2 marks
– runCalculator function 3 marks
– Type-class instances 2 marks
– Overall design, including helpful program comments 4 marks
4
Submission
Submit your .hs file(s) to the assessment folder on Blackboard. If you have a
bunch of files you can zip them up and submit them
--}

import Data.String
import Data.Eq
import Data.Ord
import Data.Bool
import Data.Char
import Text.Show

import Data.Functor
import Control.Applicative hiding ((<|>), some, many)
import Control.Monad


import System.IO
import System.Environment (getArgs)


data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

data Pair a b = Pair a b
  deriving (Eq, Show)

data NonEmptyList a = NonEmptyList a (List a)
  deriving (Eq, Show)


-- type String = [Char]
listToNativeList :: List a -> [a]
listToNativeList = foldRight (:) []


data Operation =
   Add
   | Subtract
   | Divide
   | Multiply
   deriving (Eq, Show)

data Expression =
   Number Int
   | Op Expression Operation Expression
   | Parens Expression
   deriving (Eq, Show)

data ParseResult x =
   ParseError String
   | ParseSuccess x String
   deriving (Eq, Show)

instance Functor ParseResult where
   fmap f pr = case pr of
      ParseError err -> ParseError err
      ParseSuccess x str -> ParseSuccess (f x) str

data Parser x = Parser (String -> ParseResult x)

runParser :: Parser a -> String -> ParseResult a
runParser (Parser f) = f

foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h `Cons` t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h `Cons` t) = let b' = f b h in b' `seq` foldLeft f b' t

-- XXX !!!! these instances are copied from the prac, make sure they are correct and sufficiantly different

instance Functor Parser where
   fmap f (Parser p) = Parser (\s -> fmap f (p s))

instance Applicative Parser where
   pure a = Parser (\s -> ParseSuccess a s)

   -- Might need to ask for help with this.. don't really understand it.
   -- Why would you run s on f first, rather than a on s
   (<*>) (Parser f) (Parser a) = Parser (
    \s -> case f s of
      ParseError err -> ParseError err
      ParseSuccess f s' -> case a s' of
        ParseError err -> ParseError err
        ParseSuccess a s'' -> ParseSuccess (f a) s'')

instance Monad Parser where
   return a = Parser (\s -> ParseSuccess a s)

   (>>=) (Parser a) f = Parser (\s ->
         case a s of
            ParseError err -> ParseError err
            ParseSuccess x s' -> let (Parser b) = (f x) in b s')


-- | Succeeds if string is non-empty and next Char satisfies
--   the predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser
  (\s -> case s of
    (c:rest) | pred c -> ParseSuccess c rest
    _ -> ParseError "satisfy"
  )


-- | Parse a specific character
char :: Char -> Parser Char
char c = satisfy (== c)  -- "operation section"


-- | Succeed iff all input has been consumed.
endOfInput :: Parser ()
endOfInput = Parser (\s -> case s of
  (_:_) -> ParseError "endOfInput failed"
  _ -> ParseSuccess () ""
  )


-- | If the first parser fails, try the second parser
--
-- Often pronounced "choice", "or", i.e. "this or that"
--
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser (\s -> case runParser p1 s of
  success@(ParseSuccess _ _) -> success  -- same as vvv
  -- ParseSuccess a rest -> ParseSuccess a rest   
  ParseError _err -> runParser p2 s
  )


-- | Run the parser as many times as possible;
--   must succeed at least once;
--   collect non-empty list of results.
some :: Parser a -> Parser (NonEmptyList a)
some p = NonEmptyList <$> p <*> many p    -- same as vvv
-- some p = liftA2 NonEmptyList p (many p)

-- | Run the parser as many times as possible;
--   collect list of results.
many :: Parser a -> Parser (List a)
many p = liftA2 Cons p (many p) <|> pure Nil

-- liftA2 :: (Applicative k) => (a -> b -> c) -> k a -> k b -> k c
-- liftA2 f ka kb = f <$> ka <*> kb


-- Parse zero or more values separated by something
sepBy :: Parser a -> Parser sep -> Parser (List a)
sepBy elem sep =
  liftA2 Cons elem (many (sep *> elem))
  <|> pure Nil


-- | Parse a digit ('0' .. '9')
parseDigit :: Parser Int
parseDigit = c2i <$> satisfy (\c -> c >= '0' && c <= '9')
  where
  c2i c = fromIntegral (ord c - ord '0')

-- | Parse non-signed integer
parseInteger :: Parser Int
parseInteger = f <$> some parseDigit
  where
  f (NonEmptyList x xs) = foldLeft (\n -> (n * 10 +)) 0 (x `Cons` xs)

parseSignedInteger :: Parser Int
parseSignedInteger =
  ((char '-' $> ((-1) *)) <*> parseInteger) <|> parseInteger


-- reassociate the syntax tree according to order of precedence ??
-- I think this might be like changing 2 + 5 * 3 to 5 * 3 + 2 (and then everything can be done left to right??)
reassociate :: Expression -> Expression
reassociate = error "todo"


data Triple a b c  = Triple a b c
   deriving (Eq, Show)


-- converts a string into an expression, or an error
-- infinite recursion occours on first instance of parseExpression
-- might have to have a seperate instance for term to stop recursion

{-
   T -> A o T | ( T ) | N
   A -> N | ( T )
   or, equivalently
   T -> A o T | A
   A -> N | ( T )
-}

parseExpression :: Parser Expression
parseExpression =
   (helper <$> ((liftA3 Triple) parseExtra parseOperator parseExpression))
   <|> parseExtra
   
   -- this was the old way, but uses duplicate code
   --(char '(' *> parseExpression <* char ')')
   -- <|> (Number <$> parseSignedInteger)
      
parseExtra :: Parser Expression
parseExtra = (Number <$> parseSignedInteger)
   <|> (Parens <$> (char '(' *> parseExpression <* char ')'))

-- need to think about the case where we have multiple whitespace
parseOperator :: Parser Operation
parseOperator = char ' ' *> Parser (\s ->
   case s of    
   ('+':rest) -> ParseSuccess Add rest
   ('-':rest) -> ParseSuccess Subtract rest
   ('*':rest) -> ParseSuccess Multiply rest
   ('/':rest) -> ParseSuccess Divide rest
   _ -> ParseError "Operation not defined") <* char ' '

helper :: (Triple Expression Operation Expression) -> Expression
helper (Triple exp1 op exp2) = Op exp1 op exp2

-- could make functions parseOperator, parseTerm, parseNumber, parseDigit, parseWhitespace
-- need to know how to split strings
-- would fuctor/applicative/monad be useful here?

-- removes very simple expressions
simplify :: Expression -> Expression
simplify (Op expr Add (Number 0)) = simplify expr
simplify (Op (Number 0) Add expr) = simplify expr
simplify (Op expr Subtract (Number 0)) = simplify expr
simplify (Op (Number 0) Multiply expr) = Number 0
simplify (Op expr Multiply (Number 0)) = Number 0
simplify (Op expr Multiply (Number 1)) = simplify expr
simplify (Op (Number 1) Multiply expr) = simplify expr
-- Even though expr could be 0 here, 
-- but division by 0 is undefined and 
-- can return anything so we are ok in 
-- this case
simplify (Op (Number 0) Divide expr) = Number 0 
simplify expr = expr

-- runs the property tests for simplify
runTests :: IO ()
runTests = error "todo"

-- takes an expression and reduces it to an answer
calculate :: Expression -> Int
calculate (Number n) = n
calculate (Parens exp) = calculate exp
calculate (Op exp1 op exp2) = case op of
   Add      -> value1 + value2
   Subtract -> value1 - value2
   Divide   -> value1 `div` value2
   Multiply -> value1 * value2
   where value1 = calculate exp1
         value2 = calculate exp2

-- runs the calculator interactively
runCalculator :: IO ()
runCalculator = error "todo"
   -- line <- getLine
   -- case runParser parseExpression line of
   --    ParseError err -> putStrLn err
   --    ParseSuccess exp rest -> putStrLn . show . calculate . simplify $ exp






-- Identity rules There are some simple algebraic laws that we can use to
-- simplify a calculator expression. For example, if we add or subtract 0, we know
-- that we can just use the other number. The following property tests demonstrate
-- the minimum required identity rules that should apply to the Expression
-- Use these property tests for the simplification function in your API:
-- testAddRightIdentity :: String -> Bool
-- testAddRightIdentity s =
-- simplify (Op expr Add (Number 0)) == simplify expr
-- testSubtractRightIdentity :: Expression -> Bool
-- testSubtractRightIdentity expr =
-- simplify (Op expr Subtract (Number 0)) == simplify expr
-- testMultiplyLeftZero :: Expession -> Bool
-- testMultiplyLeftZero expr =
-- simplify (Op (Number 0) Multiply expr) == Number 0
-- testMultiplyRightIdentity :: Expression -> Bool
-- testMultiplyRightIdentity expr =
-- simplify (Op expr Multiply (Number 1)) == simplify expr
-- There are a few useful simplifications that are missing from the above list. Add
-- similar simplifications and write property tests for them.
-- Import the test framework introduced during the lectures. Create a
-- genExpression :: Gen Expression to execute the property tests.
