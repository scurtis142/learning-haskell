{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Semigroup (Semigroup(..))  -- <>

-- This has the same type as the List structure from prac 9
data Logic a
   = Logic (forall r. (a -> r -> r) -> r -> r)


-- With the rankNType notation you have to generate the constructors
-- Need to change these up slightly as they are exactly the same as 
-- in the prac minus the name List -> Logic difference
nil :: Logic a
nil = Logic $ \_c n -> n

cons :: a -> Logic a -> Logic a
cons h (Logic t) = Logic $ \c n -> c h (t c n)

-- Get foldRight for free
-- Might not be needed.
foldRight :: (a -> b -> b) -> b -> Logic a -> b
foldRight f z (Logic l) = l f z


-- These instances are also exactly the same
-- if i can redo functor logic we can get rid of this
instance Semigroup (Logic a) where
  (<>) :: Logic a -> Logic a -> Logic a
  l1 <> l2 = foldRight cons l2 l1

-- see if prac is posted so we can see how to do this without foldRight
instance Functor Logic where
   fmap :: (a -> b) -> (Logic a) -> (Logic b)
   fmap f = foldRight (cons . f) nil

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

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Functor (State s) where
   fmap f (State sf) = State (\s -> 
         let (answer, newState) = sf s in (f answer, newState)
         )

instance Applicative (State s) where
   pure a = State (\s -> (a, s))

   (<*>) (State sff) (State sfa) = State (\s -> 
         let (answer, newState) = sff s 
             (a2, ns2)          = sfa newState in
             (answer a2, ns2)            
         )

instance Monad (State s) where
   return a = State (\s -> (a, s))

   (>>=) (State a) f = State (\s ->
         let (a', s') = a s
             State r = f a' in
               --f a' is of type State s b, therefore, r is of type s -> (b, s)
               r s'  
         )

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


-- Maybe delete these if they dont get used
data Index = I0 | I1 | I2 | I3
   deriving (Eq, Ord, Show)

data Indices = Index Index
   deriving (Eq, Ord, Show)

data Square = S1 | S2 | S3 | S4
   deriving (Eq, Ord, Show)

data Four a
   = Four a a a a
   deriving (Eq, Ord, Show)


instance Functor Four where
   fmap f (Four a0 a1 a2 a3) = (Four (f a0) (f a1) (f a2) (f a3))

-- For testing the foldable Four instance
testfold :: Digit -> Int -> Int
testfold D1 x = x+1
testfold D2 x = x+2
testfold D3 x = x+3
testfold D4 x = x+4

instance Foldable Four where
   foldr f b (Four a0 a1 a2 a3) = (f a0) . (f a1) . (f a2) $ f a3 b


instance Traversable Four where
   traverse f (Four a b c d) = pure Four <*> (f a) <*> (f b) <*> (f c) <*> (f d)
   --equivalent:
   -- traverse f (Four a b c d) = Four <$> (f a) <*> (f b) <*> (f c) <*> (f d)
   -- sequenceA (Four a b c d) = pure Four <*> a <*> b <*> c <*> d
   -- sequenceA (Four a b c d) = Four <$> a <*> b <*> c <*> d


data Board a
   = Board (Four (Four a))
   deriving (Eq, Ord, Show)


instance Functor Board where
   fmap f (Board (Four a b c d)) = Board (Four (fmap f a) (fmap f b) (fmap f c) (fmap f d))


instance Foldable Board where
   foldr f b (Board (Four a0 a1 a2 a3)) = foldr f (foldr f (foldr f (foldr f b a3) a2) a1) a0

instance Traversable Board where
   --traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
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


-- digit to char
digToChar :: Digit -> Char
digToChar D1 = '1'
digToChar D2 = '2'
digToChar D3 = '3'
digToChar D4 = '4'


dependant :: (Index, Index) -> (Index, Index) -> Bool
dependant (i0, j0) (i1, j1) = if (i0 == i1) && (j0 == j1) then False
                              else if i0 == i1 || j0 == j1 then True
                              else sameSquare (i0, j0) (i1, j1)

sameSquare :: (Index, Index) -> (Index, Index) -> Bool
sameSquare a b = getSquare a == getSquare b


getSquare :: (Index, Index) -> Square
getSquare (i, j) = 
   if (i == I0 || i == I1) && (j == I0 || j == I1) then S1
   else if (i == I2 || i == I3) && (j == I0 || j == I3) then S2
   else if (i == I0 || i == I1) then S3
   else S4

-- Loop through each hole in the board, getting the co-ordinates. 
-- then loop throught each hole again, and get co-ordinates. If 
-- the co-ordinates are compatible, add pair to list of constraints

bar :: Hole -> State (Index, Index) ((Index, Index), Hole)
bar hole = State (\coOrds -> ((coOrds, hole), changeCoOrds coOrds))


changeCoOrds :: (Index, Index) -> (Index, Index)
changeCoOrds coOrds = case coOrds of
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
   (I3,I3) -> (I0,I0)


secondLoop :: Board ((Index, Index), Hole) -> ((Index, Index), Hole) -> [Constraint] -> [Constraint]
secondLoop board (coOrds, hole) constraints = 
   foldr function constraints board
   where function = \(coOrdsToCheck, holeToCheck) subConstraints ->
                        if dependant coOrds coOrdsToCheck then 
                           (NotEqual hole holeToCheck):subConstraints
                        else subConstraints
   

-- List of all sudoku rules applied to a board
generateConstraints :: Board Hole -> [Constraint]
generateConstraints board = foldr (secondLoop indexedBoard) [] indexedBoard
   where (indexedBoard, _) = runState (traverse bar board) (I0, I0)


-- Creates variables for unknown cells
-- State is meant to keep track of what number we are up to to give the variable a unique identifier
-- Increment the "state" (int) for variable. Keep same for Concrete as its not used.
cellToHole :: Cell -> State Int Hole
cellToHole Unknown = State (\int -> (Variable int, int + 1))
cellToHole (Known digit) = State (\int -> (Concrete digit, int))


-- Im not sure why this works, but will create something of type
-- Board Hole from Board Cell
--(board, int) = runState (traverse cellToHole cellBoard) 0


-- Could have this as a lambda function inside substiture if nothing else uses it.
fillHole :: Int -> Digit -> Hole -> Hole
fillHole var1 digit hole = case hole of 
   Concrete _ -> hole
   Variable var2 -> if var1 == var2 then
                        Concrete digit    
                    else
                        hole

-- Replaces a variable in a board
-- Find the hole with the given int variable, then replaces that hole with a concrete digit
-- If the variable is not found then just return the board unchanged
-- More than 1 hole with a given variable can be changed
substitute :: Int -> Digit -> Board Hole -> Board Hole
substitute var digit board = fmap (fillHole var digit) board


-- Replaces a variable in a constraint
-- Not sure if this is the correct implementation. 
-- Just calling fillhole for each hole in the constraint.
instantiate :: Int -> Digit -> Constraint -> Constraint
instantiate int digit (NotEqual hole1 hole2) =
   let fh = fillHole int digit in NotEqual (fh hole1) (fh hole2)


-- If no variables exist, emits the board
-- Otherwise solves the board's next variable
solver :: [Constraint] -> Board Hole -> Logic (Board Digit)
solver = error "todo"


-- Runs all of the above together
sudoku :: Board Cell -> Logic (Board Digit)
-- (board, int) = runState (traverse cellToHole cellBoard) 0
sudoku = error "todo"


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


prettyBoard :: (a -> Char) -> Board a -> String
prettyBoard s (Board (Four a b c d)) =
   prettyFour s a b ++ prettyFour s c d


cellBoard :: Board Cell
cellBoard = Board (Four (Four Unknown Unknown (Known D1) Unknown)
                   (Four Unknown Unknown (Known D2) Unknown)
                   (Four Unknown Unknown (Known D3) Unknown)
                   (Four Unknown (Known D2) (Known D4) Unknown))
