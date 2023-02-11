module Direction where

data Direction
    = N | S | W  | E
    deriving Eq

instance Show Direction where
    show anything = case anything of
        N -> "north"
        S -> "south"
        E -> "east"
        W -> "west"

directions = [N,S,E,W]
