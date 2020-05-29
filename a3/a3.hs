{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}


{- IMPORTS -}

import Data.Semigroup (Semigroup(..))  -- <>
import Prelude hiding (tail)


{- TYPES, CLASSES AND INSTANCES -}

-- This has the same type as the List structure from prac 9
data Logic a
   = Logic (forall r. (a -> r -> r) -> r -> r)


-- With the rankNType notation you have to generate the constructors
-- Need to change these up slightly as they are exactly the same as 
-- in the prac minus the name List -> Logic difference

-- Constructor for Logic type
nil :: Logic a
nil = Logic $ \_c n -> n


-- Constructor for Logic type
cons :: a -> Logic a -> Logic a
cons h (Logic t) = Logic $ \c n -> c h (t c n)


instance Foldable Logic where
   foldr f z (Logic l) = l f z


-- These instances are also exactly the same
-- if i can redo functor logic we can get rid of this
instance Semigroup (Logic a) where
  (<>) :: Logic a -> Logic a -> Logic a
  l1 <> l2 = foldr cons l2 l1


-- see if prac is posted so we can see how to do this without foldr
instance Functor Logic where
   fmap :: (a -> b) -> (Logic a) -> (Logic b)
   fmap f = foldr (cons . f) nil


instance Applicative Logic where
   pure :: a -> Logic a
   pure a = cons a nil

   (<*>) :: Logic (a -> b) -> Logic a -> Logic b
   Logic fs <*> as = fs (\f r -> fmap f as <> r) nil--see if we can do this without <>


instance Monad Logic where
  (>>=) :: Logic a -> (a -> Logic b) -> Logic b
  Logic as >>= f = as (\a r -> f a <> r) nil


-- These ones should be good
data State s a =
   State (s -> (a, s))


instance Functor (State s) where
   fmap f (State sf) = State (\s -> 
         let (answer, newState) = sf s in (f answer, newState))


instance Applicative (State s) where
   pure a = State (\s -> (a, s))

   (<*>) (State sff) (State sfa) = State (\s -> 
         let (answer, newState) = sff s 
             (a2, ns2)          = sfa newState in
             (answer a2, ns2)            )


instance Monad (State s) where
   return a = State (\s -> (a, s))

   (>>=) (State a) f = State (\s ->
         let (a', s') = a s
             State r = f a' in
               --f a' is of type State s b, therefore, r is of type s -> (b, s)
               r s'  )


data Digit
   = D1
   | D2
   | D3
   | D4
   deriving (Eq, Ord, Show)


data Cell
   = Unknown
   | Known Digit
   deriving (Eq, Ord, Show)


data Index = I0 | I1 | I2 | I3
   deriving (Eq, Ord, Show)


data Square = S1 | S2 | S3 | S4
   deriving (Eq, Ord, Show)


data Four a
   = Four a a a a
   deriving (Eq, Ord, Show)


instance Functor Four where
   fmap f (Four a0 a1 a2 a3) = (Four (f a0) (f a1) (f a2) (f a3))


instance Foldable Four where
   foldr f b (Four a0 a1 a2 a3) = (f a0) . (f a1) . (f a2) $ f a3 b


instance Traversable Four where
   traverse f (Four a b c d) =
      pure Four <*> (f a) <*> (f b) <*> (f c) <*> (f d)


data Board a
   = Board (Four (Four a))
   deriving (Eq, Ord, Show)


instance Functor Board where
   fmap f (Board (Four a b c d)) =
      Board (Four (fmap f a) (fmap f b) (fmap f c) (fmap f d))


instance Foldable Board where
   foldr f b (Board (Four a0 a1 a2 a3)) =
      foldr f (foldr f (foldr f (foldr f b a3) a2) a1) a0


instance Traversable Board where
    traverse f (Board (Four a b c d)) =
      let a' = traverse f a
          b' = traverse f b
          c' = traverse f c
          d' = traverse f d in
           pure Board <*> (pure Four <*> a' <*> b' <*> c' <*> d')


data Hole
   = Concrete Digit
   | Variable Int
   deriving (Eq, Ord, Show)


data Constraint
   = NotEqual Hole Hole
   deriving (Eq, Ord, Show)


{- FUNCTIONS -}

-- Runs the state function
runState :: State s a -> s -> (a, s)
runState (State f) = f


-- Returns True if 2 sets of indices are dependant. i.e. in the same row
-- column or sub-square. Will return false if equal, as we don't want a 
-- constraint from itself to itself. Also will only return True for one
-- of 'dependant a b' and 'dependant b a' as to not duplicate constraints.
dependant :: (Index, Index) -> (Index, Index) -> Bool
-- It doesn't actually matter how the < function is defined on indices. 
-- As long as (a < b) != (b < a).
dependant a@(x0, y0) b@(x1, y1) = if a < b then False
   else if x0 == x1 && y0 == y1 then False   -- not dependant on itself 
   else if x0 == x1 || y0 == y1 then True    -- Same row or column
   else getSquare a == getSquare b           -- Same sub-square


-- Returns the sub-square in which this set of indices resides
getSquare :: (Index, Index) -> Square
getSquare (x, y) = 
   if      (x == I0 || x == I1) && (y == I0 || y == I1) then S1
   else if (x == I2 || x == I3) && (y == I0 || y == I1) then S2
   else if (x == I0 || x == I1) then S3
   else S4


-- Creates indices for a Board Hole
-- This assumes the coOrds given to state will be in the order of
-- traversal of a board. This function converts that order to x,y
-- co-ordinates on the grid
indexBoard :: Hole -> State (Index, Index) ((Index, Index), Hole)
indexBoard hole = State (\coOrds -> ((coOrds, hole), case coOrds of
   (I0,I0) -> (I1,I0)
   (I1,I0) -> (I0,I1)
   (I0,I1) -> (I1,I1)
   (I1,I1) -> (I2,I0)
   (I2,I0) -> (I3,I0)
   (I3,I0) -> (I2,I1)
   (I2,I1) -> (I3,I1)
   (I3,I1) -> (I0,I2)
   (I0,I2) -> (I1,I2)
   (I1,I2) -> (I0,I3)
   (I0,I3) -> (I1,I3)
   (I1,I3) -> (I2,I2)
   (I2,I2) -> (I3,I2)
   (I3,I2) -> (I2,I3)
   (I2,I3) -> (I3,I3)
   (I3,I3) -> (I0,I0)))


-- Appends all dependancies for a single hole to the list of constraints
appendConstraints :: Board ((Index, Index), Hole) -> ((Index, Index), Hole) 
   -> [Constraint] -> [Constraint]
appendConstraints board (coOrds, hole) constraints = 
   foldr appendIfDependant constraints board
   where appendIfDependant = \(coOrdsToCheck, holeToCheck) subConstraints ->
                        if dependant coOrds coOrdsToCheck then 
                           (NotEqual hole holeToCheck):subConstraints
                        else subConstraints
   

-- List of all sudoku rules applied to a board
generateConstraints :: Board Hole -> [Constraint]
generateConstraints board = foldr (appendConstraints indexedBoard) [] indexedBoard
   where (indexedBoard, _) = runState (traverse indexBoard board) (I0, I0)


-- Returns True if all the constraints hold. False otherwise
assertConstraints :: [Constraint] -> Bool
assertConstraints [] = True
assertConstraints ((NotEqual h1 h2):tail) = (h1 /= h2) && assertConstraints tail


-- Creates variables for unknown cells
-- Keep track of what number we are up to to give a unique identifier
cellToHole :: Cell -> State Int Hole
cellToHole Unknown = State (\int -> (Variable int, int + 1))
cellToHole (Known digit) = State (\int -> (Concrete digit, int))


-- If the given int matches the varibale in the given hole, update
-- the hole with the given digit.
fillHole :: Int -> Digit -> Hole -> Hole
fillHole _ _ hole@(Concrete _) = hole
fillHole var1 digit hole@(Variable var2) = if var1 == var2 
   then Concrete digit
   else hole


-- Replaces a variable in a board
-- If the variable is not found then just return the board unchanged
-- More than 1 hole with a given variable can be changed
substitute :: Int -> Digit -> Board Hole -> Board Hole
substitute var digit board = fmap (fillHole var digit) board


-- Replaces a variable in a constraint
instantiate :: Int -> Digit -> Constraint -> Constraint
instantiate int digit (NotEqual hole1 hole2) =
   let fh = fillHole int digit in NotEqual (fh hole1) (fh hole2)


-- Converts hole to Either Int Digit
holeToEitherIntDigit :: Hole -> Either Int Digit
holeToEitherIntDigit (Concrete digit) = Right digit
holeToEitherIntDigit (Variable int) = Left int


-- Used in solver to iterate over the digits
digits :: Logic Digit
digits = cons D1 (cons D2 (cons D3 (cons D4 nil)))


-- If no variables exist, emits the board
-- Otherwise solves the board's next variable
-- Return value is all the possible board digits that could exist 
-- given the inputs. e.g. Nil for an impossible board, [b1, b2, ... , bn]
-- for a board with n possible solutions.
solver :: [Constraint] -> Board Hole -> Logic (Board Digit)
solver constraints board = 
   let nextOrfinish = traverse holeToEitherIntDigit board in
      case nextOrfinish of
         -- If there are no more variable, return the board. else loop through
         -- and try all possible digits
         (Right digitBoard) -> cons digitBoard nil
         (Left nextVar) -> digits >>= \nextDigit -> 
            let newConstraints = fmap (instantiate nextVar nextDigit) constraints
                newBoard = substitute nextVar nextDigit board in
                  case assertConstraints newConstraints of
                     True -> solver newConstraints newBoard     
                     False -> nil


-- Runs all of the above together
sudoku :: Board Cell -> Logic (Board Digit)
sudoku cellBoard = let (holeBoard, _) = runState (traverse cellToHole cellBoard) 0
                       constraints = generateConstraints holeBoard in
                       solver constraints holeBoard


{- TESTING -}

-- A board to test
testBoard :: Board Cell
testBoard = Board (Four (Four Unknown Unknown (Known D1) Unknown)
                   (Four Unknown Unknown (Known D2) Unknown)
                   (Four Unknown Unknown (Known D3) Unknown)
                   (Four Unknown (Known D2) (Known D4) Unknown))
   

-- Digit to char function
digToChar :: Digit -> Char
digToChar D1 = '1'
digToChar D2 = '2'
digToChar D3 = '3'
digToChar D4 = '4'


-- Good for visualising a Four
prettyFour :: (a -> Char) -> Four a -> Four a -> String
prettyFour s (Four a b c d) (Four e f g h) =
   unlines
      [ "/--\\/--\\"
      , "|" ++ [s a, s b] ++ "||"
            ++ [s e, s f] ++ "|"
      , "|" ++ [s c, s d] ++ "||"
            ++ [s g, s h] ++ "|"
      , "\\--/\\--/"
      ]


-- Good for visualising a Board
prettyBoard :: (a -> Char) -> Board a -> String
prettyBoard s (Board (Four a b c d)) =
   prettyFour s a b ++ prettyFour s c d


-- Appends a board to a string
showBoards :: Board Digit -> String -> String
showBoards board string = unlines [string, prettyBoard digToChar board]


-- To test the example board
main :: IO ()
main = putStr $ foldr showBoards "" (sudoku testBoard)
