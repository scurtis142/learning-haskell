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

-- XXX !!!! these instances are copied from the prac, make sure they are correct and sufficiantly different

instance Functor ParseResult where
   fmap f pr = case pr of
      ParseError err -> ParseError err
      ParseSuccess x str -> ParseSuccess (f x) str


data Parser x = Parser (String -> ParseResult x)


instance Functor Parser where
   fmap f (Parser p) = Parser (\s -> fmap f (p s))


instance Applicative Parser where
   pure a = Parser (\s -> ParseSuccess a s)

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


{- PRIVATE FUNCTIONS -}


runParser :: Parser a -> String -> ParseResult a
runParser (Parser f) = f


foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h `Cons` t) = f h (foldRight f b t)


foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h `Cons` t) = let b' = f b h in b' `seq` foldLeft f b' t


-- Succeeds if string is non-empty and next Char satisfies
-- the predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser
  (\s -> case s of
    (c:rest) -> if pred c then ParseSuccess c rest else
                  ParseError ("Parse error: unexpected '" ++ [c] ++ "'")
    _ -> ParseError "Parse error: unexpected end of input")


-- Parse a specific character
char :: Char -> Parser Char
char c = satisfy (== c)


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
sepBy elem sep =
  liftA2 Cons elem (many (sep *> elem))
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

{- API FUNCTIONS -}

-- Reassociate the syntax tree according to order of precedence
-- e.g. Op (Op (Number 3) Add (Number 5)) Multiply (Number 8)
--   to Op (Number 3) Add (Op (Number 5) Multiply (Number 8))
reassociate :: Expression -> Expression
reassociate (Number n) = Number n
reassociate (Parens exp) = Parens (reassociate exp)
reassociate (Op exp1 op1 (Op exp2 op2 exp3)) = if op1 >= op2
   then (Op (Op (reassociate exp1) op1 (reassociate exp2)) op2 (reassociate exp3))
   else (Op (reassociate exp1) op1 (Op (reassociate exp2) op2 (reassociate exp3)))
reassociate (Op (Op exp1 op1 exp2) op2 exp3) = if op1 >= op2
   then (Op (Op (reassociate exp1) op1 (reassociate exp2)) op2 (reassociate exp3))
   else (Op (reassociate exp1) op1 (Op (reassociate exp2) op2 (reassociate exp3)))
reassociate (Op exp1 op exp2) = Op (reassociate exp1) op (reassociate exp2)


{-
   Changed the grammer slighlty to remove ambiguity
   Still has same functionality

   Term        -> Term' Operator Term | Term'
   Term'       -> Number | '(' Term ')'
   Operator    -> Whitespace Operator' Whitespace
   Operator'   -> '+' | '-' | '*' | '/'
   Number      -> ['-'] Digit+
   Digit       -> '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'
   Whitespace  -> ' '+

   Note, this means we will recurse to the right, i.e. get
   syntax trees of the form (Op exp1 op1 (Op exp2 op2 exp3))
-}

-- Converts a string into an expression, or an error
parseExpression :: Parser Expression
parseExpression = reassociate <$>
   (liftA3 Op parseExtra parseOperator parseExpression)
   <|> parseExtra
      

-- Parse either a Number or Expression in parenthesis
parseExtra :: Parser Expression
parseExtra = (Number <$> parseSignedInt)
   <|> (Parens <$> (char '(' *> parseExpression <* char ')'))


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
calculate (Parens exp) = calculate exp
calculate (Op exp1 op exp2) = case op of
   Add      -> value1 + value2
   Subtract -> value1 - value2
   Divide   -> value1 `div` value2
   Multiply -> value1 * value2
   where value1 = calculate exp1
         value2 = calculate exp2


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
genOperation = Gen (\_ gen -> let (int, newGen) = next gen in
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
