module Main (main) where

import Play
import Uno

-- main function to run the game
main :: IO TournammentState
main = do 
    -- unos <- uno_start
    start_state <- uno_win_start
    play uno start_state simple_player (0,0)





