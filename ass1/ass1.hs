import Data.Char


-- a data type that denotes the numbers that each player may select from
data Number_1to9 =
   N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
   deriving (Eq,Show)


num129_to_int :: Number_1to9 -> Int
num129_to_int N1 = 1
num129_to_int N2 = 2
num129_to_int N3 = 3
num129_to_int N4 = 4
num129_to_int N5 = 5
num129_to_int N6 = 6
num129_to_int N7 = 7
num129_to_int N8 = 8
num129_to_int N9 = 9


int_to_num129 :: Int -> Number_1to9
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


is_num129 :: Char -> Bool
is_num129 x = if x > '0' && x <= '9' then True else False

-- is_num129 :: Int -> Bool
-- is_num129 x = if x > 0 && x < 10 then True else False


instance Num Number_1to9 where
   (+) = \a b -> int_to_num129 (num129_to_int a + num129_to_int b)
   (-) = \a b -> int_to_num129 (num129_to_int a - num129_to_int b)
   (*) = \a b -> int_to_num129 (num129_to_int a * num129_to_int b)
   negate = error "Cant negate Number_1to9"
   abs n = n
   signum n = 1
   fromInteger = int_to_num129 . fromIntegral


--type Number_1to9 = Int

-- need to write a comment explaining this.
-- not needed as going to use the inbuild haskell list function
--data List a =
--   Nil | Cons a (List a)


-- starts off empty and records each move as a player selects a number
data GameState =
   GameState [Number_1to9]
   deriving Show


-- can probably roll this up into 1 type
type PlayerMoves = [Number_1to9]


data Solution =
   Solution
      Number_1to9
      Number_1to9
      Number_1to9
   deriving (Eq)

instance Show Solution where
   show (Solution n1 n2 n3) = show (num129_to_int n1) ++ " + "
      ++ show (num129_to_int n2) ++ " + "
      ++ show (num129_to_int n3) ++ " = "
      ++ show (num129_to_int n1 + num129_to_int n2 + num129_to_int n3)

-- the possible results of a valid number selection
data ValidSelection a =
   Player1Wins Solution
   | Player2Wins Solution
   | Draw
   | KeepPlaying a
   deriving Show


-- the possible results of any number selection
data NumberSelected =
   InvalidSelection
   | AlreadySelected Number_1to9
   | Selected (ValidSelection GameState)
   deriving Show


-- start a new game
newGame :: GameState
newGame = GameState []


getPlayer1Moves :: GameState -> PlayerMoves
getPlayer1Moves (GameState []) = []
getPlayer1Moves (GameState (x:[])) = x:[]
getPlayer1Moves (GameState (x:y:z)) = x:getPlayer1Moves (GameState z)


getPlayer2Moves :: GameState -> PlayerMoves
getPlayer2Moves (GameState []) = []
getPlayer2Moves (GameState (x:xs)) = getPlayer1Moves (GameState xs)


player1check :: GameState -> Maybe Solution
player1check = getSolution . getPlayer1Moves


player2check :: GameState -> Maybe Solution
player2check = getSolution . getPlayer2Moves


-- fix ugly syntax, could use 'let in'
-- select a number from the game state
selectNumber :: Number_1to9 -> GameState -> NumberSelected
selectNumber n (GameState state)
   | n `elem` state = AlreadySelected n
   | player1won /= Nothing = case player1won  of
      (Just solution) -> Selected $ Player1Wins solution
   | player2won /= Nothing = case player2won of
      (Just solution) -> Selected $ Player2Wins solution
   | length (state ++ [n]) == 9 = Selected Draw
   | otherwise      = Selected (KeepPlaying (GameState (state ++ [n])))
   where player1won = player1check (GameState (state ++ [n]))
         player2won = player2check (GameState (state ++ [n]))


getSolution :: PlayerMoves -> Maybe Solution
getSolution moves = case testWin of
   Nothing  -> Nothing
   (Just (n1:n2:n3:[])) -> Just (Solution (int_to_num129 n1) (int_to_num129 n2) (int_to_num129 n3))
   (Just x) -> error ("Internal error: '" ++ show x ++ "' not a solution with 3 integers")
   where testWin = sumNEqual (map num129_to_int moves) 15 3


-- returns whether or not the game state is a draw
isDraw :: GameState -> Bool
isDraw (GameState state) = player1check (GameState state) == Nothing
                           && player2check (GameState state) == Nothing
                           && length state == 9


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
sumNEqual (x:xs) goal count = case withHead of
   (Just a) -> (Just (x:a))
   Nothing  -> withoutHead
   where withHead    = sumNEqual xs (goal - x) (count-1)
         withoutHead = sumNEqual xs goal count


playPick15 :: IO ()
playPick15 = do
   putStrLn "press 'q' to Quit"
   mainLoop newGame


mainLoop :: GameState -> IO ()
mainLoop game = do
   printState game
   putStrLn $ "Player " ++ (calcPlayerTurn game) ++ " to move"
   putStr ">>> "
   input <- getChar
   putStrLn ""
   case input of
      'q' -> do
         putStrLn "Bye!"
      _   -> do
         if is_num129 input then
            case selectNumber (int_to_num129 $ digitToInt input) game of
               AlreadySelected _ -> do
                  putStrLn "Already selected"
                  mainLoop game
               Selected (Player1Wins solution) -> putStrLn $ "Player 1 wins! " ++ show solution
               Selected (Player2Wins solution) -> putStrLn $ "Player 2 wins!" ++ show solution
               Selected Draw -> putStrLn "The game is a draw"
               Selected (KeepPlaying newState) -> mainLoop newState
               InvalidSelection -> error "Internal Error"
         else do
            putStrLn "Invalid input"
            mainLoop game


calcPlayerTurn :: GameState -> String
calcPlayerTurn (GameState state) = if length state `mod` 2 == 1 then "2" else "1"


printState :: GameState -> IO ()
printState (GameState state) = let numbers = [N1,N2,N3,N4,N5,N6,N7,N8,N9] in
   do
      putStr "["
      printStateHelper state numbers
      putStrLn "]"


printStateHelper :: [Number_1to9] -> [Number_1to9] -> IO ()
printStateHelper state [] = return ()
printStateHelper state (x:xs) = if x `elem` state then do
   if xs /= [] then putStr "  " else putStr " "
   printStateHelper state xs
   else do
     if xs /= [] then putStr $ number ++ " " else putStr $ number
     printStateHelper state xs
   where number = show (num129_to_int x)
