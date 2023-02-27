module Main (main) where

-- stack install random
-- add random to dependencies in package.yaml

import Play
import MagicSum



main = do
    unos <- uno_start
    play uno unos simple_player (0,0,0)

{-main = do
    move <- simple_player uno_test
    putStrLn (show move)
--    putStrLn (show (length phandd))-}

{-main = do
    (ContinueGame True (State (phand, ohand, top))) <- uno (return 2) uno_test
    ohandd <- sequence ohand
    top_c <- top
    phandd <- sequence phand
    putStrLn (show top_c)
    putStrLn (show (length ohandd))
    putStrLn (show (length phandd))-}


{-main = do
    (ContinueGame True (State (phand, ohand, top))) <- uno 2 uno_test
--    top_card <- top
    opponent_hand <- sequence ohand
--    putStrLn (show top_card)
    putStrLn (show (length opponent_hand))
-}

{- main = do
    (ContinueGame True (State (phand, ohand, top))) <- uno 0 uno_start
    (EndOfGame double state) <- uno 1 (State (phand, ohand, top))
    let win = double in
        putStrLn (show win) -}


{-main = do
    (ContinueGame True (State (phand, ohand, top))) <- uno 0 uno_start
    (ContinueGame True (State (phand2, ohand2, top2))) <- uno 20 (State (phand, ohand, top))
    pcard <- phand !! 6
    ocard <- ohand !! 7
    putStrLn (show pcard) 
    putStrLn (show ocard) -}

{- main = do
    let (ContinueGame (State (phand, ohand, top))) = uno 0 uno_start
    ocard <- ohand !! 7
    putStrLn (show ocard) -}



{-main = do
    let State (phand, ohand, top) = uno_start
    top_card <- top
    value <- win phand
    putStrLn (show value) -}


{-main = do
    let State (phand, ohand, top) = uno_start
    value <- check_valid phand 1 top
    putStrLn (show value) -}
    
{- main = do
    let State (phand, ohand, top) = uno_start
    top_card <- phand !! 6
    putStrLn (show top_card) -}
--main = do
--     card <- (generate_card)
--     putStrLn (show card)
-- main = play magicsum magicsum_start simple_player (0,0,0)
