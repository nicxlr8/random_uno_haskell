-- Templated from https://www.cs.ubc.ca/~poole/cs312/2023/haskell/Play.hs

module Play (module Play) where

import Uno

import Text.Read   (readMaybe)

-- a tournament state
type TournammentState = (Int,Int)   -- wins, losses

-- plays the game and returns the state of the tournament
play :: Game -> State -> Player -> TournammentState -> IO TournammentState
play game start_state opponent ts =
  let (wins, losses) = ts in
  do
    putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses")
    putStrLn "Who starts? 0=you, 1=computer, 2=exit."
    line <- getLine
    if line == "0"
        then
            person_play game (ContinueGame True (return start_state)) opponent ts
        else if line ==  "1"
            then computer_play game (ContinueGame True (return start_state)) opponent ts
        else if line == "2"
            then return ts
        else play game start_state opponent ts

-- plays the person's move
person_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
person_play game (ContinueGame _ state) opponent ts =
    do
      (State (mhand, chand, top)) <- state
      myhand <- sequence mhand
      top_c <- top
      putStrLn("")
      putStrLn ("Top card is: " ++ show top_c)
      putStrLn ("Your hand: " ++ show myhand)
      putStrLn ("To draw a card enter: 0, To play a card enter the position of the card in your hand ")
      line <- getLine
      case (readMaybe line :: Maybe Int) of
            Nothing ->
                do
                    putStrLn ("Please enter an integer")
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
                                let (valid, result_state) = extract_bool_state presult
                                (State (rchand, rmhand, rtop)) <- result_state
                                if (valid)
                                    then computer_play game (ContinueGame valid result_state) opponent ts
                                    else person_play game (ContinueGame valid (return (State (rmhand, rchand, rtop)))) opponent ts
                                    
-- defines the tournament state for a person's winning move
person_play game (EndOfGame val io_start_state) opponent ts =
  do
    start_state <- io_start_state
    newts <- update_tournament_state (val) ts  -- val is value to computer; -val is value for person
    play game start_state opponent newts

-- extract boolean and state from a Result
extract_bool_state :: Result -> (Bool, IO State)
extract_bool_state (EndOfGame _ _) = (True, uno_start)
extract_bool_state (ContinueGame valid result_state) = (valid, result_state)                               

-- plays the computer's move                                                
computer_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
computer_play game (EndOfGame val io_start_state) opponent ts =
    do  
        start_state <- io_start_state
        newts <- update_tournament_state (-val) ts
        play game start_state opponent newts
        
-- defines the tournament state for a computer's winning move
computer_play game (ContinueGame _ io_state) opponent ts = do
      (State (chand, mhand, top)) <- io_state
      let 
          opponent_move = opponent (State (chand, mhand, top)) -- Computer plays a move
        in
          do
            putStrLn("")
            putStrLn ("Computer has "++ show (length chand) ++ " cards")
            cresult <- (game opponent_move (State (chand, mhand, top)))
            if ((showWhat cresult) == "End")
                then
                    do
                        computer_play game cresult opponent ts
                else
                    do
                        let (valid, result_state) = extract_bool_state cresult
                        (State (rmhand, rchand, rtop)) <- result_state
                        if (valid)
                            then person_play game (ContinueGame valid result_state) opponent ts
                            else computer_play game (ContinueGame valid (return (State (rchand, rmhand, rtop)))) opponent ts

-- given value to the person, the tournament state, return the new tournament state
update_tournament_state:: Double -> TournammentState -> IO TournammentState
update_tournament_state val (wins,losses)
  | val > 0 = do
      putStrLn "You Won"
      return (wins+1,losses)
  | otherwise = do
      putStrLn "Computer won!"
      return (wins,losses+1)
      





