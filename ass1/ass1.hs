import Data.Char  -- to use digitToInt
import Data.List  -- to use \\ (list differnece)


----------------- GLOBAL DEFINITIONS -----------------
global_target = 15
global_num_moves = 3
global_draw_length = 9

------------------------------------------------------

-------- DATA TYPES, INSTANCES AND TYPECLASSES -------

-- A data type that denotes the numbers that each player may select from
data Number_1to9 =
   N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9
   deriving (Eq,Show)


-- Convers a Number_1to9 to Int
num129_to_int :: Number_1to9 -> Int
num129_to_int n
   | n == N1 = 1 | n == N2 = 2 | n == N3 = 3
   | n == N4 = 4 | n == N5 = 5 | n == N6 = 6
   | n == N7 = 7 | n == N8 = 8 | n == N9 = 9


-- Convers a Int to Number_1to9 
int_to_num129 :: Int -> Number_1to9
int_to_num129 n
   | n == 1 = N1 | n == 2 = N2 | n == 3 = N3
   | n == 4 = N4 | n == 5 = N5 | n == 6 = N6
   | n == 7 = N7 | n == 8 = N8 | n == 9 = N9
   | otherwise = error ((show n) ++ " is not a Number_1to9")

-- Returns True if the given char associates with a Number_1to9
is_num129 :: Char -> Bool
is_num129 x = if x > '0' && x <= '9' then True else False


-- instance Num Number_1to9 where
--    (+) = \a b -> int_to_num129 (num129_to_int a + num129_to_int b)
--    (-) = \a b -> int_to_num129 (num129_to_int a - num129_to_int b)
--    (*) = \a b -> int_to_num129 (num129_to_int a * num129_to_int b)
--    negate n = int_to_num129 $ 10 - num129_to_int n
--    abs n = n
--    signum n = 1
--    fromInteger = int_to_num129 . fromIntegral


-- I have chosen to use the Haskell built in List data type, as that is 
-- the syntax that will be used for other Haskell code that i might work
-- on outside this course, so it is good to get used to using that.

-- Starts off empty and records each move as a player selects a number
data GameState =
   GameState [Number_1to9]
   deriving Show


-- Type synonym for a players moves, which is a list of Number_1to9
type PlayerMoves = [Number_1to9]


-- Data type representing a player, There are only 2 players in this game
data Player = Player1 | Player2


-- Override show function here so we can print the correct string for 
-- diplaying the player turn in the main loop of the program.
instance Show Player where
   show player = case player of
      Player1 -> "Player 1"
      Player2 -> "Player 2"


-- Data type representing a winning solution to the game
data Solution =
   Solution
      Number_1to9
      Number_1to9
      Number_1to9
   deriving (Eq)


-- Override show here so we can use the show method in the terminal game
-- to print the correct information to the screen
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

------------------------------------------------------


----------------------- API --------------------------

-- Given a gamestate, returns the valid moves a player can make
-- Example: getValidMoves (GameState [N1,N3,N4]) = [N2,N5,N6,N7,N8,N9]
getValidMoves :: GameState -> PlayerMoves
getValidMoves (GameState game) = [N1,N2,N3,N4,N5,N6,N7,N8,N9] \\ game


-- Given a player and a gamestate, returns a list of moves that player has made
-- Example: getPlayerMoves Player1 (GameState [N1,N3,N4]) = [N1,N4]
getPlayerMoves :: Player -> GameState -> PlayerMoves
getPlayerMoves player (GameState []) = []
getPlayerMoves Player2 (GameState (x:xs)) = getPlayerMoves Player1 (GameState xs)
getPlayerMoves _ (GameState (x:[])) = x:[]
getPlayerMoves _ (GameState (x:y:xs)) = x:getPlayerMoves Player1 (GameState xs)


-- Given a player and a gamestate, will return Just Solution if that player has 
-- winning numbers, otherwise Nothing.
-- Example: playerCheck Player1 (GameState [N1,N3,N4]) = Nothing
-- Example: playerCheck Player2 (GameState [N1,N3,N4,N5,N8,N7]) = Just 3 + 5 + 7 = 15
playerCheck :: Player -> GameState -> Maybe Solution
playerCheck = \player -> getSolution . (getPlayerMoves player)


-- Given a gamestate, returns the player whos turn it is.
-- Example: calcPlayerTurn (GameState [N1,N3,N4]) = Player 2
calcPlayerTurn :: GameState -> Player
calcPlayerTurn (GameState state) = if length state `mod` 2 == 1 then Player2 else Player1


-- Select a number from the game state
-- Example: selectNumber N1 (GameState [N1,N3,N4]) = AlreadySelected N1
-- Example: selectNumber N2 newGame = Selected (KeepPlaying (GameState [N1]))
-- Example: selectNumber N5 (GameState [N1,N9,N2,N8,N3,N7,N4,N6]) = Selected Draw
-- Example: selectNumber N5 (GameState [N1,N2,N9,N3]) = Selected (Player1Wins 1 + 9 + 5 = 15)
selectNumber :: Number_1to9 -> GameState -> NumberSelected
selectNumber n (GameState state)
   | n `elem` state = AlreadySelected n
   | p1MaybeSolution /= Nothing = let (Just solution) = p1MaybeSolution in Selected $ Player1Wins solution
   | p2MaybeSolution /= Nothing = let (Just solution) = p2MaybeSolution in Selected $ Player2Wins solution
   | length (state ++ [n]) == global_draw_length = Selected Draw
   | otherwise      = Selected (KeepPlaying (GameState (state ++ [n])))
   where p1MaybeSolution = playerCheck Player1 (GameState (state ++ [n]))
         p2MaybeSolution = playerCheck Player2 (GameState (state ++ [n]))


-- Given a Player's moves, return a Just solution if they have a winning solution, Nothing otherwise
-- Example: getSolution [N1,N5,N9] = Just 1 + 5 + 9 = 15
-- Example: getSolution [N1,N5] = Nothing
getSolution :: PlayerMoves -> Maybe Solution
getSolution moves = case testWin of
   Nothing  -> Nothing
   (Just (n1:n2:n3:[])) -> Just (Solution (int_to_num129 n1) (int_to_num129 n2) (int_to_num129 n3))
   where testWin = sumNEqual (map num129_to_int moves) global_target global_num_moves


-- Returns whether or not the game state is a draw
-- Example: isDraw (GameState [N1,N9,N2,N8,N3,N7,N4,N6,N5]) = True
-- Example: isDraw (GameState [N1,N9,N2,N8,N3,N7,N4,N6]) = False
isDraw :: GameState -> Bool
isDraw (GameState state) = playerCheck Player1 (GameState state) == Nothing
                           && playerCheck Player2 (GameState state) == Nothing
                           && length state == global_draw_length


-- Given a list, a target and a count to reach that target, return Just a list 
-- of size count containing elements that sum to target, otherwise, Nothing.
-- The function was designed to be general enough that it will still work if the
-- game rules were expanded enough to be a different goal number or count.
-- Example: sumNEqual [1,4,9,5] 15 3 = Just [1,9,5]
-- Example: sumNEqual [1,4,9,5] 12 2 = Nothing
sumNEqual :: (Num t, Eq t) => [t] -> t -> t -> Maybe [t]
sumNEqual [] 0 0 = Just []
sumNEqual [] _ _ = Nothing
sumNEqual list goal 1 = if goal `elem` list then Just [goal] else Nothing
sumNEqual (head:tail) goal count = case includeHead of
   (Just a) -> (Just (head:a))
   Nothing  -> excludeHead
   where includeHead = sumNEqual tail (goal - head) (count-1)
         excludeHead = sumNEqual tail goal count

------------------------------------------------------

------------------- TERMINAL GAME --------------------

-- Plays the game on a terminal (stdout)
playPick15 :: IO ()
playPick15 = do
   putStrLn "press 'q' to Quit"
   mainLoop newGame


-- Main loop of the terminal game, takes the current GameState, called recursivly with a new GameState.
mainLoop :: GameState -> IO ()
mainLoop game = do
   -- Prints state, playerturn , prompt and gets input
   printState game
   putStrLn $ (show (calcPlayerTurn game)) ++ " to move"
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
               Selected (Player2Wins solution) -> putStrLn $ "Player 2 wins! " ++ show solution
               Selected Draw -> putStrLn "The game is a draw"
               Selected (KeepPlaying newState) -> mainLoop newState  -- Repeat with new State
               InvalidSelection -> error "Internal Error"
         else do
            putStrLn "Invalid input"
            mainLoop game


-- Prints the game's state to stdout as as list of avaliable numbers to pick
printState :: GameState -> IO ()
printState (GameState state) = let numbers = [N1,N2,N3,N4,N5,N6,N7,N8,N9] in
   do
      putStr "["
      printStateHelper state numbers
      putStrLn "]"


-- Helper function for printing the state. Prints space seperated numbers
-- that appear in numbers but not state, replaces the number with a space
-- if it does appear
printStateHelper :: [Number_1to9] -> [Number_1to9] -> IO ()
printStateHelper state [] = return ()
printStateHelper state (x:xs) = if x `elem` state then do
   -- If we are at the end of the list, we don't want an extra space
   if xs /= [] then putStr "  " else putStr " "
   printStateHelper state xs
   else do
     -- If we are at the end of the list, we don't want an extra space
     if xs /= [] then putStr $ number ++ " " else putStr $ number
     printStateHelper state xs
   where number = show (num129_to_int x)
