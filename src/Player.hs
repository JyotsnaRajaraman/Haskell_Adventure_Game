module Player where

import Item
import Room

import Data.List

data Player = Player { inventory :: [ItemName],
                        maxWeight :: Integer,
                        location :: RoomName 
                    }
            deriving (Show, Eq)

addItem :: ItemName -> Player -> Player

addItem name Player {inventory, maxWeight, location} = Player { inventory = inventory ++ [name],  maxWeight, location}


removeItem :: ItemName -> Player -> Player

removeItem name Player {inventory, maxWeight, location} = Player { inventory = delete name inventory,  maxWeight, location}

newLocation :: RoomName -> Player -> Player

newLocation name Player {inventory, maxWeight, location} = Player { inventory, maxWeight, location = name}

isCarryingAnything :: Player -> Bool

isCarryingAnything Player {inventory, maxWeight, location} = if inventory == [] then False else True

you :: Player

you = Player [] 30 Outside
