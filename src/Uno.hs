-- Templated from https://www.cs.ubc.ca/~poole/cs312/2023/haskell/MagicSum.hs

module Uno (module Uno) where
import System.Random
import Data.Char

-- a colour of a Card
data Colour = Colour Char
        deriving (Eq)

-- a card
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

-- show colour as a String    
instance Show Colour where
    show (Colour colour) = show colour

-- the game state
data State = State InternalState  

-- the result: whether the game is finished or not
data Result = EndOfGame Double (IO State)    -- end of game: value, starting state
            | ContinueGame Bool (IO State)   -- continue game: flag, state
            
-- creates a string from a Result    
showWhat :: Result -> String    
showWhat (EndOfGame _ _) = "End"
showWhat (ContinueGame _ _) = "Continue"

-- game
type Game = Action -> State -> IO Result

-- player
type Player = State -> Action

-- action
type Action = IO Int

-- internal_state
type InternalState = ([IO Card],  [IO Card], IO Card)


-- returns colour of a Card
get_colour :: Card -> Colour
get_colour (Card col _) = col

-- returns number of a Card
get_number :: Card -> Int
get_number (Card _ number) = number

-- checks if an Action is valid in the current game's State and returns the Result
check_valid :: Int -> State -> IO Result
check_valid move (State (phand, ohand, top_c)) = do
    hand <- sequence phand
    top <- top_c
    if ((move > 0) && move <= (length hand))
        then do
            return (ContinueGame ((get_colour (hand !! (move-1)) == get_colour top) || (get_number (hand !! (move-1)) == get_number top) || (get_colour (hand !! (move-1)) == (Colour 'W'))) (return (State (phand, ohand, top_c))))
        else return (ContinueGame False (return (State (phand, ohand, top_c))))

-- plays the game using an Action and a State and returns a Result
uno :: Game
uno io_move (State (mine,others, top)) = do
        move <- io_move
        (ContinueGame valid _) <- (check_valid move (State(mine, others, top)))
        if (move > 0)
            then do
                    if (valid)
                        then do
                            winner <- (win mine)
                            if (winner)
                                then return (EndOfGame 10 uno_start)     -- agent wins
                                else do
                                    played <- (play_card (State (mine,others,top)) move)
                                    return played
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

-- plays a card and updates the InternalState and returns corresponding Result
play_card :: State -> Int -> IO Result
play_card (State (phand, ohand, top_c)) move = do
    player_hand <- sequence phand
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
            putStrLn("Pick a color: R, B, Y, G")
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

-- removes the nth number from a list
remove_nth :: (Num t, Ord t) => t -> [a] -> [a]
remove_nth 0 (h:t) = (h:t)
remove_nth _ [] = []
remove_nth n (h:t)
    | n-1>0 = h:remove_nth (n-1) t
    | otherwise = remove_nth (n-1) t

-- checks if hand has 1 card
win :: (Monad m, Traversable t) => t (m a) -> m Bool
win phand  =
    do
        hand <- sequence phand
        return (length hand == 1)


-- generates the start state
uno_start :: IO State
uno_start = do
    number1 <- getStdRandom (randomR (1, 112))
    number2 <- getStdRandom (randomR (10,12))
    number3 <- getStdRandom (randomR (13,14))
    
    number4 <- getStdRandom (randomR (1, 112))
    number5 <- getStdRandom (randomR (10,12))
    number6 <- getStdRandom (randomR (13,14))
    
    number7 <- getStdRandom (randomR (1, 112))
    number8 <- getStdRandom (randomR (10,12))
    number9 <- getStdRandom (randomR (13,14))
    
    number10 <- getStdRandom (randomR (1, 112))
    number11 <- getStdRandom (randomR (10,12))
    number12 <- getStdRandom (randomR (13,14))
    
    number13 <- getStdRandom (randomR (1, 112))
    number14 <- getStdRandom (randomR (10,12))
    number15 <- getStdRandom (randomR (13,14))
    
    number16 <- getStdRandom (randomR (1, 112))
    number17 <- getStdRandom (randomR (10,12))
    number18 <- getStdRandom (randomR (13,14))
    
    number19 <- getStdRandom (randomR (1, 112))
    number20 <- getStdRandom (randomR (10,12))
    number21 <- getStdRandom (randomR (13,14))
    
    number22 <- getStdRandom (randomR (1, 112))
    number23 <- getStdRandom (randomR (10,12))
    number24 <- getStdRandom (randomR (13,14))
    
    number25 <- getStdRandom (randomR (1, 112))
    number26 <- getStdRandom (randomR (10,12))
    number27 <- getStdRandom (randomR (13,14))
    
    number28 <- getStdRandom (randomR (1, 112))
    number29 <- getStdRandom (randomR (10,12))
    number30 <- getStdRandom (randomR (13,14))
    
    number31 <- getStdRandom (randomR (1, 112))
    number32 <- getStdRandom (randomR (10,12))
    number33 <- getStdRandom (randomR (13,14))
    
    number34 <- getStdRandom (randomR (1, 112))
    number35 <- getStdRandom (randomR (10,12))
    number36 <- getStdRandom (randomR (13,14))
    
    number37 <- getStdRandom (randomR (1, 112))
    number38 <- getStdRandom (randomR (10,12))
    number39 <- getStdRandom (randomR (13,14))
    
    number40 <- getStdRandom (randomR (1, 112))
    number41 <- getStdRandom (randomR (10,12))
    number42 <- getStdRandom (randomR (13,14))
    
    numbertop <- getStdRandom (randomR (1, 80))

    
    return (State ((generate_card number1 number2 number3):(generate_card number4 number5 number6):(generate_card number7 number8 number9):(generate_card number10 number11 number12):(generate_card number13 number14 number15):(generate_card number16 number17 number18):(generate_card number19 number20 number21):[] , (generate_card number22 number23 number24):(generate_card number25 number26 number27):(generate_card number28 number29 number30):(generate_card number31 number32 number33):(generate_card number34 number35 number36):(generate_card number37 number38 number39):(generate_card number40 number41 number42):[] , (generate_top numbertop)))

-- generates a winning start for the player state
uno_win_start:: IO State
uno_win_start = do
    return (State ((return (Card (Colour 'R') 12)) : (return (Card (Colour 'R') 10)) : (return (Card (Colour 'R') 6)) : [], (return (Card (Colour 'G') 1)) : (return (Card (Colour 'B') 2)) : (return (Card (Colour 'Y') 6)) : [], (return (Card (Colour 'R') 7))))

-- Simple player for the game   
simple_player :: Player
-- this player has cards, and chooses the first valid card based on the top card
simple_player (State (mine, _, top)) = do
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

-- finds the index of the head of a list in another list
find_card :: (Num t, Eq t, Eq a) => t -> [a] -> [a] -> t                 
find_card n [_] [_] = n
find_card _ [] [] = 0
find_card _ [] (_:_) = 0
find_card _ (_:_) [] = 0  
find_card n (h:t) (x:y)
    | h==x = n
    | otherwise = find_card (n+1) (h:t) (y)

-- finds out if the card is playable
possible_move :: Card -> Card -> Bool
possible_move (Card col num) (Card top_col top_num)
    | (col == top_col && col /= (Colour 'W')) = True
    | (num == top_num && (num /= 13 && num /= 14)) = True
    | otherwise = False

-- generates a random top card given a seed for the start state
generate_top :: Int -> IO Card
generate_top rand_num = 
    do
        if (rand_num <=20)
            then return (Card (Colour 'Y') (rand_num `mod` 10))
            else if (rand_num > 20 && rand_num <= 40)
                then return (Card (Colour 'B') (rand_num `mod` 10))
            else if (rand_num > 40 && rand_num <=60)
                then return (Card (Colour 'R') (rand_num `mod` 10))
            else return (Card (Colour 'G') (rand_num `mod` 10))


-- generates a random card given a seed
generate_card :: Int -> Int -> Int -> IO Card
generate_card rand_num rand_spec rand_wild = 
    do
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



