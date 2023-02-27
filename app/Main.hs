module Main (main) where

-- stack install random
-- add random to dependencies in package.yaml

import Play
import Uno



main = do
    unos <- uno_start
    play uno unos simple_player (0,0,0)





