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

-- not needed as going to use the inbuild haskell list function
--data List a =
--   Nil | Cons a (List a)

-- starts off empty and records each move as a player selects a number
data GameState =
   GameState [Number_1to9]

data Solution =
   Solution
      Number_1to9
      Number_1to9
      Number_1to9

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
--newGame :: GameState
--newGame = []

-- select a number from the game state
--selectNumber :: Number_1to9 -> GameState -> NumberSelected
--selectNumber number state = if number `elem` state then  AlreadySelected number else number : state

-- returns whether or not the game state is a draw
--isDraw :: GameState -> Bool

-- Basic implementation, need to review edge cases and the fact that it's super inefficient
sumNEqual :: [Integer] -> Integer -> Integer -> Bool
sumNEqual [] 0 0 = True
sumNEqual [] _ _ = False
sumNEqual list goal 1 = if goal `elem` list then True else False
sumNEqual (x:xs) goal count = if sumNEqual xs (goal - x) (count-1) then True else sumNEqual xs goal count


--playPick15 :: IO ()


-- Not actually sure if we need this one. 

-- Determines if the game is finished by any state.
-- Useful function to implement isDraw.
--isGameOver :: GameState -> Bool



