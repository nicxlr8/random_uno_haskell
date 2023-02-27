-- CPSC 312 - 2023 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play


import MagicSum
-- import CountGame

-- import Minimax  -- make sure same game is imported in Minimax
--import Minimax_mem


import System.Random
import System.IO
import Text.Read   (readMaybe)


-- ‘Y’ = 1-20  mod 10 - 1
--        then (Card (Colour 'Y') (rand_num `mod` 10) -1)
--          putStrLn ( show (Card (Colour 'Y') ((rand_num `mod` 10) -1)))
type TournammentState = (Int,Int,Int)   -- wins, losses, ties


play :: Game -> State -> Player -> TournammentState -> IO TournammentState

play game start_state opponent ts =
  let (wins, losses, ties) = ts in
  do
    putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
    putStrLn "Who starts? 0=you, 1=computer, 2=exit."
    line <- getLine
    if line == "0"
        then
            person_play game (ContinueGame True uno_start) opponent ts
        else if line ==  "1"
            then computer_play game (ContinueGame True uno_start) opponent ts
        else if line == "2"
            then return ts
        else play game start_state opponent ts

person_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play

person_play game (ContinueGame val state) opponent ts =
    do
      (State (mhand, chand, top)) <- state
      myhand <- sequence mhand
      top_c <- top
      putStrLn ("Top card is: " ++ show top_c)
      putStrLn ("Your hand: " ++ show myhand)
      line <- getLine
      case (readMaybe line :: Maybe Int) of
            Nothing ->
                do
                    putStrLn ("Please enter an integer :)")
                    person_play game (ContinueGame True (return (State (mhand, chand, top)))) opponent ts
            Just action ->
                do
                    presult <- (game (return action) (State (mhand, chand, top)))
                    if ((showWhat presult) == "End")
                        then
                            do
                                person_play game presult opponent ts
                        else
                            do
                                let (ContinueGame valid result_state) = presult
                                (State (rchand, rmhand, rtop)) <- result_state
                                if (valid)
                                    then computer_play game presult opponent ts
                                    else person_play game (ContinueGame valid (return (State (rmhand, rchand, rtop)))) opponent ts
                        
        

person_play game (EndOfGame val io_start_state) opponent ts =
  do
    start_state <- io_start_state
    newts <- update_tournament_state (val) ts  -- val is value to computer; -val is value for person
    play game start_state opponent newts

computer_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent ts
-- person has played, the computer must now play
computer_play game (EndOfGame val io_start_state) opponent ts =
    do  
        start_state <- io_start_state
        newts <- update_tournament_state (-val) ts
        play game start_state opponent newts

computer_play game (ContinueGame val io_state) opponent ts = do
      (State (chand, mhand, top)) <- io_state
      let 
          opponent_move = opponent (State (chand, mhand, top)) -- Computer plays a move
        in
          do
            putStrLn ("Computer has "++ show (length chand))
            cresult <- (game opponent_move (State (chand, mhand, top)))
            if ((showWhat cresult) == "End")
                then
                    do
                        computer_play game cresult opponent ts
                else
                    do
                        let (ContinueGame valid result_state) = cresult
                        (State (rmhand, rchand, rtop)) <- result_state
                        if (valid)
                            then person_play game cresult opponent ts
                            else computer_play game (ContinueGame valid (return (State (rchand, rmhand, rtop)))) opponent ts

update_tournament_state:: Double -> TournammentState -> IO TournammentState
-- given value to the person, the tournament state, return the new tournament state
update_tournament_state val (wins,losses,ties)
  | val > 0 = do
      putStrLn "You Won"
      return (wins+1,losses,ties)
  | val == 0 = do
      putStrLn "It's a tie"
      return (wins,losses,ties+1)
  | otherwise = do
      putStrLn "Computer won!"
      return (wins,losses+1,ties)

-- If you imported MagicSum here and in Minimax try:
-- play magicsum magicsum_start simple_player (0,0,0)
-- play magicsum magicsum_start (mm_player magicsum) (0,0,0) -- minimax player

-- If you imported CountGameNew here and in Minimax_mem try:
-- let (cg, ss) = createCountGame 20 [1,2,3,5,7] in play cg ss (simple_count_player 20 [1,2,3,5,7]) (0,0,0)
-- let (cg, ss) = createCountGame 20 [1,2,3,5,7] in play cg ss (mm_player cg) (0,0,0) 




















