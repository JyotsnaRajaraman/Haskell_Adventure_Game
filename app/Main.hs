module Main where
import Control.Monad.State

import GameState
import GameIO

main :: IO ()

main = putStrLn "This is your nightmare!!" >> evalStateT (forever repl) initialState
