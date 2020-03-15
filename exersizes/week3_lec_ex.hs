-- adds up the numbers in a list
sum :: List Integer -> Integer
-- appends two lists
(++) :: List x -> List x -> List x
-- flattens a list of lists
flatten :: List (List x) -> List x
-- reverses a list
reverse :: List x -> List x

modifyA :: (a -> a) -> Or a b -> Or a b
getB :: Or a b -> Optional b

mapTree :: (a -> b) -> Tree a -> Tree b
traverseTreeOptional ::
  (a -> Optional b) -> Tree a -> Optional (Tree b)
  errlog ($ERR_USER, "got here 1.0");
  errlog ($ERR_USER, "got here 1.0");
  errlog ($ERR_USER, "got here 1.0");
  errlog ($ERR_USER, "got here 1.0");
  errlog ($ERR_USER, "got here 1.0");
  errlog ($ERR_USER, "got here 1.0");


-- the error message
parseError :: ParseResult x -> Optional String
-- return a parser that succeeds, puts the input to the output
neutralParser :: x -> Parser x
-- a parser that consumes one character of input (if it can)
character :: Parser Char


maximumV6 :: Order a => Vector6 a -> a
reverseV6 :: Order a => Vector6 a -> Vector6 a
