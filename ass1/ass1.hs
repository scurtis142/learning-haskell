import Data.Char

-- TODO
-- need to write a comment explaining why going to use the inbuild haskell list function
-- remove redundant player1 2 code
-- comments

-- A data type that denotes the numbers that each player may select from
data Number_1to9 =
   N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
   deriving (Eq,Show)


num129_to_int :: Number_1to9 -> Int
num129_to_int n
   | n == N1 = 1 | n == N2 = 2 | n == N3 = 3
   | n == N4 = 4 | n == N5 = 5 | n == N6 = 6
   | n == N7 = 7 | n == N8 = 8 | n == N9 = 9


int_to_num129 :: Int -> Number_1to9
int_to_num129 n
   | n == 1 = N1 | n == 2 = N2 | n == 3 = N3
   | n == 4 = N4 | n == 5 = N5 | n == 6 = N6
   | n == 7 = N7 | n == 8 = N8 | n == 9 = N9
   | otherwise = error ((show n) ++ " is not a Number_1to9")


is_num129 :: Char -> Bool
is_num129 x = if x > '0' && x <= '9' then True else False


instance Num Number_1to9 where
   (+) = \a b -> int_to_num129 (num129_to_int a + num129_to_int b)
   (-) = \a b -> int_to_num129 (num129_to_int a - num129_to_int b)
   (*) = \a b -> int_to_num129 (num129_to_int a * num129_to_int b)
   negate = error "Can't negate Number_1to9"
   abs n = n
   signum n = 1
   fromInteger = int_to_num129 . fromIntegral


-- Starts off empty and records each move as a player selects a number
data GameState =
   GameState [Number_1to9]
   deriving Show


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

-- The possible results of a valid number selection
data ValidSelection a =
   Player1Wins Solution
   | Player2Wins Solution
   | Draw
   | KeepPlaying a
   deriving Show


-- The possible results of any number selection
data NumberSelected =
   InvalidSelection
   | AlreadySelected Number_1to9
   | Selected (ValidSelection GameState)
   deriving Show


-- Start a new game
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


-- select a number from the game state
selectNumber :: Number_1to9 -> GameState -> NumberSelected
selectNumber n (GameState state)
   | n `elem` state = AlreadySelected n
   | p1MaybeSolution /= Nothing = let (Just solution) = p1MaybeSolution in Selected $ Player1Wins solution
   | p2MaybeSolution /= Nothing = let (Just solution) = p2MaybeSolution in Selected $ Player2Wins solution
   | length (state ++ [n]) == 9 = Selected Draw
   | otherwise      = Selected (KeepPlaying (GameState (state ++ [n])))
   where p1MaybeSolution = player1check (GameState (state ++ [n]))
         p2MaybeSolution = player2check (GameState (state ++ [n]))

getSolution :: PlayerMoves -> Maybe Solution
getSolution moves = case testWin of
   Nothing  -> Nothing
   (Just (n1:n2:n3:[])) -> Just (Solution (int_to_num129 n1) (int_to_num129 n2) (int_to_num129 n3))
   where testWin = sumNEqual (map num129_to_int moves) 15 3


-- Returns whether or not the game state is a draw
isDraw :: GameState -> Bool
isDraw (GameState state) = player1check (GameState state) == Nothing
                           && player2check (GameState state) == Nothing
                           && length state == 9


-- Type
-- Type names
sumNEqual :: (Num t, Eq t) => [t] -> t -> t -> Maybe [t]
sumNEqual [] 0 0 = Just []
sumNEqual [] _ _ = Nothing
sumNEqual list goal 1 = if goal `elem` list then Just [goal] else Nothing
sumNEqual (head:tail) goal count = case includeHead of
   (Just a) -> (Just (head:a))
   Nothing  -> excludeHead
   where includeHead = sumNEqual tail (goal - head) (count-1)
         excludeHead = sumNEqual tail goal count


-- Plays the game on a terminal (stdout)
playPick15 :: IO ()
playPick15 = do
   putStrLn "press 'q' to Quit"
   mainLoop newGame


mainLoop :: GameState -> IO ()
mainLoop game = do
   -- Prints state, playerturn , prompt and gets input
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


-- Given a gamestate, calculates which players turn it is.
-- Returns string for convinience with printing.
calcPlayerTurn :: GameState -> String
calcPlayerTurn (GameState state) = if length state `mod` 2 == 1 then "2" else "1"


-- Prints the game's state to stdout as as list of avaliable numbers to pick
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
