-- CPSC 312 - 2023 - Games in Haskell
module MagicSum where

-- To run it, try:
-- ghci
-- :load MagicSum

import System.Random
import Data.Char


data Colour = Colour Char
        deriving (Eq)

data Card = Card Colour Int
        deriving (Eq)
        
-- show cards as a tuple (Colour, Int or String)
instance Show Card where
    show (Card colour num)
        | (num < 10) = show (colour, num)
        | (num == 10) = show (colour, "Skip")
        | (num == 11) = show (colour, "Reverse")
        | (num == 12) =  show (colour, "+2")
        | (num == 13) = show (colour, "CC")
        | otherwise = show (colour, "+4 CC")

    
instance Show Colour where
    show (Colour colour) = show colour


data State = State InternalState  -- internal_state

data Result = EndOfGame Double (IO State)    -- end of game: value, starting state
            | ContinueGame Bool (IO State) 
            
showWhat (EndOfGame arg arg2) = "End"
showWhat (ContinueGame arg arg2) = "Continue"

type Game = Action -> State -> IO Result

type Player = State -> Action

------ The UNO Game -------

type Action = IO Int

-- (your action, other action)
type InternalState = ([IO Card],  [IO Card], IO Card)


--magicsum :: Game
--magicsum move (State (mine,others) available) 
--  | win move mine                = EndOfGame 1  magicsum_start     -- agent wins
--    | available == [move]          = EndOfGame 0  magicsum_start     -- no more moves, tie
--    | otherwise                    =
--          ContinueGame (State (others,(move:mine))   -- note roles have flipped
 --                       [act | act <- available, act /= move])

get_colour (Card col number) = col

get_number (Card col number) = number

check_valid move (State (phand, ohand, top_c)) = do
    hand <- sequence phand
    top <- top_c
    if ((move > 0) && move <= (length hand))
        then do
            return (ContinueGame ((get_colour (hand !! (move-1)) == get_colour top) || (get_number (hand !! (move-1)) == get_number top) || (get_colour (hand !! (move-1)) == (Colour 'W'))) (return (State (phand, ohand, top_c))))
        else return (ContinueGame False (return (State (phand, ohand, top_c))))


uno :: Game
uno io_move (State (mine,others, top)) = do
        move <- io_move
        (ContinueGame valid state) <- (check_valid move (State(mine, others, top)))
        if (move > 0)
            then do
                    if (valid)
                        then do
                            winner <- (win mine)
                            if (winner)
                                then return (EndOfGame 69 uno_start)     -- agent wins
                                else do
                                    played <- (play_card (State (mine,others,top)) move)
                                    return played
                                    --return (ContinueGame True (State (mine, others, top)))   -- invalid move, loop back    
                             {-  else
                        -- Change state based on card    -- TOOOOOOOODOOOOOOOO
                        --             return ContinueGame (State (others,(move:mine), (mine !! (move - 1))))   -- note roles   -}
                         else return (ContinueGame False (return (State (others, mine, top))))   -- invalid move, loop back   
            else if (move == 0) -- move is Draw
                then do
                    number1 <- getStdRandom (randomR (1, 112))
                    number8 <- getStdRandom (randomR (10,12))
                    number15 <- getStdRandom (randomR (13,14))
                    let draw_card = generate_card number1 number8 number15 in
                        return (ContinueGame True (return (State (others, (draw_card:mine), top))))   -- roles have flipped
            else 
                return (ContinueGame False (return (State (others, mine, top))))   -- invalid move, loop back

play_card (State (phand, ohand, top_c)) move = do
    player_hand <- sequence phand
    opponent_hand <- sequence ohand
    let top = (player_hand !! (move - 1))
    let removed_player_hand = remove_nth move phand
    if (get_number top < 10)
        then
            return (ContinueGame True (return (State (ohand, removed_player_hand, (return top)))))
        else if (get_number top == 10)
            then return (ContinueGame False (return (State (ohand, removed_player_hand, (return top)))))
        else if (get_number top == 11)
            then return (ContinueGame True (return (State (ohand, removed_player_hand, (return top)))))
        else if (get_number top == 12)  
            then do 
                number1 <- getStdRandom (randomR (1, 112))
                number8 <- getStdRandom (randomR (10,12))
                number16 <- getStdRandom (randomR (13,14))
                number2 <- getStdRandom (randomR (1, 112))
                number9 <- getStdRandom (randomR (10,12))
                number17 <- getStdRandom (randomR (13,14))
                return (ContinueGame False (return (State ((generate_card number1 number8 number16):(generate_card number2 number9 number17):ohand, removed_player_hand, (return top)))))
        else do 
            putStrLn("Pick a color")
            rline <- getLine
            let line = (head rline)
            if (((length rline) == 1) && (toUpper line == 'Y' || toUpper line == 'G' || toUpper line == 'B' || toUpper line == 'R' ))
                then
                    if (get_number top == 13)
                        then return (ContinueGame True (return (State (ohand, removed_player_hand, (return (Card (Colour (toUpper line)) 13))))))
                        else do
                            number1 <- getStdRandom (randomR (1, 112))
                            number8 <- getStdRandom (randomR (10,12))
                            number16 <- getStdRandom (randomR (13,14))
                            number2 <- getStdRandom (randomR (1, 112))
                            number9 <- getStdRandom (randomR (10,12))
                            number17 <- getStdRandom (randomR (13,14))
                            number3 <- getStdRandom (randomR (1, 112))
                            number10 <- getStdRandom (randomR (10,12))
                            number18 <- getStdRandom (randomR (13,14))
                            number4 <- getStdRandom (randomR (1, 112))
                            number11 <- getStdRandom (randomR (10,12))
                            number19 <- getStdRandom (randomR (13,14))
                            return (ContinueGame False (return (State ((generate_card number1 number8 number16):(generate_card number2 number9 number17):(generate_card number3 number10 number18):(generate_card number4 number11 number19):ohand, removed_player_hand ,(return (Card (Colour (toUpper line)) 14)))))) 
            else do
                putStrLn("Invalid selection")
                return (ContinueGame False (return (State (ohand,  phand, top_c))))


remove_nth 0 (h:t) = (h:t)
remove_nth _ [] = []
remove_nth n (h:t)
    | n-1>0 = h:remove_nth (n-1) t
    | otherwise = remove_nth (n-1) t


--uno_test = do
--    let phand = generate_card : generate_card : generate_card : generate_card : generate_card : generate_card : generate_card : []
--    let ohand = generate_card : generate_card : generate_card : generate_card : generate_card : generate_card : generate_card : []
--    let top = generate_card
--    State (phand, ohand, top)

-- win n ns = the agent wins if it selects n and ns is a list of [n]
win phand  =
    do
        hand <- sequence phand
        return (length hand == 1)




uno_start = do
    number1 <- getStdRandom (randomR (1, 112))
    number2 <- getStdRandom (randomR (1, 112))
    number3 <- getStdRandom (randomR (1, 112))
    number4 <- getStdRandom (randomR (1, 112))
    number5 <- getStdRandom (randomR (1, 112))
    number6 <- getStdRandom (randomR (1, 112))
    number7 <- getStdRandom (randomR (1, 112))
    number8 <- getStdRandom (randomR (10,12))
    number9 <- getStdRandom (randomR (10,12))
    number10 <- getStdRandom (randomR (10,12))
    number11 <- getStdRandom (randomR (10,12))
    number12 <- getStdRandom (randomR (10,12))
    number13 <- getStdRandom (randomR (10,12))
    number14 <- getStdRandom (randomR (10,12))
    number15 <- getStdRandom (randomR (13,14))
    number16 <- getStdRandom (randomR (13,14))
    number17 <- getStdRandom (randomR (13,14))
    number18 <- getStdRandom (randomR (13,14))
    number19 <- getStdRandom (randomR (13,14))
    number20 <- getStdRandom (randomR (13,14))
    number21 <- getStdRandom (randomR (13,14))
    return (State ((generate_card number1 number8 number15):(generate_card number2 number9 number16):(generate_card number3 number10 number17):[], (generate_card number4 number11 number18):(generate_card number5 number12 number19):(generate_card number6 number13 number20):[] , (generate_card number7 number14 number21)))


--magicsum_start = State ([],[]) [Action n | n <- [1..9]]

-- show and read actions just as the integer

------- A Player -------

--simple_player :: Player
-- this player has an ordering of the moves, and chooses the first one available
--simple_player (State _ avail) = head [Action e | e <- [5,6,4,2,8,1,3,7,9],
--                                               Action e `elem` avail]


-- Test cases
-- magicsum (simple_player magicsum_start) magicsum_start
--a i = Action i  -- make it easier to type
--as lst = [Action i | i <- lst]
-- magicsum (a 6) (State (as [3,5], as [2,7]) (as [1,4,6,8,9])) 
-- magicsum (a 3) (State (as [5,7], as [2,9]) (as [1,3,4,6,8])) 


{-simple_player :: Player
-- this player has an ordering of the moves, and chooses the first one available
simple_player (State (mine, other, top)) = do
        top_card <- top
        my_cards <- sequence mine
        let playable =  [e | e <- my_cards, possible_move e top_card]
        if (not ([] == playable))
                then do
                    return 1
                else do
                    return 0 -}
                    
simple_player :: Player
-- this player has an ordering of the moves, and chooses the first one available
simple_player (State (mine, other, top)) = do
        top_card <- top
        my_cards <- sequence mine
        let playable =  [e | e <- my_cards, possible_move e top_card]
        if (not ([] == playable))
                then do
                    putStrLn ("Computer played " ++ show (head playable))
                    return (find_card 1 playable my_cards)
                else do
                    putStrLn ("Computer drew a card")
                    return 0
                    
find_card 0 [] [] = 0
find_card n (h:t) (x:y)
    | h==x = n
    | otherwise = find_card (n+1) (h:t) (y)


possible_move (Card col num) (Card top_col top_num)
    | (col == top_col && col /= (Colour 'W')) = True
    | (num == top_num && (num /= 13 && num /= 14)) = True
    | otherwise = False


generate_card :: Int -> Int -> Int -> IO Card
generate_card rand_num rand_spec rand_wild = 
    do
        --rand_num <- getStdRandom (randomR (1, 112))
        --rand_spec <- getStdRandom (randomR (10,12))
        --rand_wild <- getStdRandom (randomR (13,14))
        if (rand_num <=20)
            then return (Card (Colour 'Y') (rand_num `mod` 10))
            else if (rand_num > 20 && rand_num <= 40)
                then return (Card (Colour 'B') (rand_num `mod` 10))
            else if (rand_num > 40 && rand_num <=60)
                then return (Card (Colour 'R') (rand_num `mod` 10))
            else if (rand_num > 60  && rand_num <=80)
                then return (Card (Colour 'G') (rand_num `mod` 10))
            else if (rand_num > 80 && rand_num <=86)
                then return (Card (Colour 'Y') rand_spec)
            else if (rand_num > 86 && rand_num <=92)
                then return (Card (Colour 'B') rand_spec)
            else if (rand_num > 92 && rand_num <=98)
                then return (Card (Colour 'R') rand_spec)
            else if (rand_num > 98 && rand_num <=104)
                then return (Card (Colour 'G') rand_spec)
            else return (Card (Colour 'W') rand_wild)


