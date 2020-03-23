{-|
   Need to investigate:
      IO ()
      getChar :: IO Char
      putStr :: String -> IO ()
      putStrLn :: String -> IO ()
|-}


-- a data type that denotes the numbers that each player may select from
data Number_1to9 =
   N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
   deriving (Eq,Show)

num129_to_int :: Number_1to9 -> Integer
num129_to_int N1 = 1
num129_to_int N2 = 2
num129_to_int N3 = 3
num129_to_int N4 = 4
num129_to_int N5 = 5
num129_to_int N6 = 6
num129_to_int N7 = 7
num129_to_int N8 = 8
num129_to_int N9 = 9

int_to_num129 :: Integer -> Number_1to9
int_to_num129 1 = N1
int_to_num129 2 = N2
int_to_num129 3 = N3
int_to_num129 4 = N4
int_to_num129 5 = N5
int_to_num129 6 = N6
int_to_num129 7 = N7
int_to_num129 8 = N8
int_to_num129 9 = N9
int_to_num129 x = error ((show x) ++ " is not a Number_1to9")


instance Num Number_1to9 where
   (+) = \a b -> int_to_num129 (num129_to_int a + num129_to_int b)
   (-) = \a b -> int_to_num129 (num129_to_int a - num129_to_int b)
   (*) = \a b -> int_to_num129 (num129_to_int a * num129_to_int b)
   negate = error "Cant negate Number_1to9"
   abs n = n
   signum n = 1
   fromInteger = int_to_num129


--type Number_1to9 = Integer

-- need to write a comment explaining this.
-- not needed as going to use the inbuild haskell list function
--data List a =
--   Nil | Cons a (List a)

-- starts off empty and records each move as a player selects a number
data GameState =
   GameState [Number_1to9]

type Player1Moves = [Number_1to9]
type Player2Moves = [Number_1to9]

data Solution =
   Solution
      Number_1to9
      Number_1to9
      Number_1to9
   deriving Show

-- the possible results of a valid number selection
data ValidSelection a =
   Player1Wins Solution
   | Player2Wins Solution
   | Draw
   | KeepPlaying a

-- the possible results of any number selection
data NumberSelected =
   InvalidSelection
   | AlreadySelected Number_1to9
   | Selected (ValidSelection GameState)


-- start a new game
newGame :: GameState
newGame = GameState []

getPlayer1Moves :: GameState -> Player1Moves
getPlayer1Moves (GameState []) = []
getPlayer1Moves (GameState (x:[])) = x:[]
getPlayer1Moves (GameState (x:y:z)) = x:getPlayer1Moves (GameState z)

getPlayer2Moves :: GameState -> Player2Moves
getPlayer2Moves (GameState []) = []
getPlayer2Moves (GameState (x:[])) = []
getPlayer2Moves (GameState (x:y:z)) = y:getPlayer2Moves (GameState z)

-- select a number from the game state
selectNumber :: Number_1to9 -> GameState -> NumberSelected
selectNumber n state = if n `elem` state 
                           then AlreadySelected n
                           else if 

-- returns whether or not the game state is a draw
--isDraw :: GameState -> Bool

-- Edge cases
-- Efficiancy
-- Variable Names
-- Elem
-- Type
-- Type names
sumNEqual :: (Num t, Eq t) => [t] -> t -> t -> Maybe [t]
sumNEqual [] 0 0 = Just []
sumNEqual [] _ _ = Nothing
sumNEqual list goal 1 = if goal `elem` list then Just [goal] else Nothing
sumNEqual (x:xs) goal count = case first of
   (Just a) -> (Just (x:a))
   Nothing  -> second
   where first  = sumNEqual xs (goal - x) (count-1)
         second = sumNEqual xs goal count


--playPick15 :: IO ()


-- Determines if the game is finished by any state.
-- Useful function to implement isDraw.
-- isGameOver :: GameState -> Bool
