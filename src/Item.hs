module Item where

import qualified Data.Map as M



type Universe = M.Map ItemName Item

data Item = Item {iname :: ItemName,
                    weight :: Integer,
                    value :: Integer}
             deriving (Show, Eq)

data ItemName
  = Bed
    | Lamp
    | Toothbrush
    | Toothpaste
    | Floss 
    | Box
    | Dumbell
    | Chest
    | Watch
    | Photoframe
    | Couch
    | Table
    | Pen
    | Rope
    | NothingThere
    | Needle
    | Key
    | Sword
  deriving (Eq, Ord)


instance Show ItemName where
    show anything = case anything of
        Lamp -> "lamp"
        NothingThere -> "nothing there"
        Rope -> "rope"
        Toothbrush -> "toothbrush"
        Toothpaste -> "toothpaste"
        Floss -> "floss"
        Box -> "box"
        Dumbell -> "dumbell"
        Chest -> "chest"
        Watch -> "watch"
        Photoframe -> "photoframe"
        Couch -> "couch"
        Table -> "table"
        Pen -> "pen"
        Needle -> "needle"
        Key -> "key"
        Bed -> "bed"
        Sword -> "sword"



mkUniverse :: [Item] -> Universe
makeIntoList :: [Item] ->  [(ItemName, Item)]
makeIntoList list = map generateTuple list
    where generateTuple (Item iname weight value) = (iname, Item iname weight value) 

listOfItems :: [(ItemName, Item)]
listOfItems = makeIntoList itemList
mkUniverse lst = M.fromList (makeIntoList lst)

itemList = [Item {iname = Table, weight= 45, value = 11},
    Item {iname = Couch, weight= 45, value = 11},
    Item {iname = Sword, weight= 65, value = 11},
    Item {iname = Chest, weight= 55, value = 11},
    Item {iname = Bed, weight= 65, value = 11},
    Item {iname = Watch, weight= 16, value = 0},
    Item {iname = Toothbrush, weight= 16, value = 1},
    Item {iname = Toothpaste, weight= 17, value = 2},
    Item {iname = Floss, weight= 16, value = 3},
    Item {iname = Needle, weight= 15, value = 4},
    Item {iname = Lamp, weight= 25, value = 5},
    Item {iname = Box, weight= 20, value = 6},
    Item {iname = NothingThere, weight= 0, value = 0},
    Item {iname = Rope, weight= 20, value = 7},
    Item {iname = Photoframe, weight= 17, value = 7},
    Item {iname = Dumbell, weight= 20, value = 8},
    Item {iname = Key, weight= 15, value = 1},
    Item {iname = Sword, weight= 20, value = 9},
    Item {iname = Pen, weight= 16, value = 10}]
univ = mkUniverse itemList

itemNames =  M.keys (univ)


monsterItemList = [Item {iname = Table, weight= 45, value = 11},
    Item {iname = Couch, weight= 45, value = 11},
    Item {iname = Sword, weight= 65, value = 11},
    Item {iname = Chest, weight= 55, value = 11},
    Item {iname = Bed, weight= 65, value = 11},
    Item {iname = Watch, weight= 16, value = 0},
    Item {iname = Toothbrush, weight= 16, value = 1},
    Item {iname = Toothpaste, weight= 17, value = 2},
    Item {iname = Floss, weight= 16, value = 3},
    Item {iname = Needle, weight= 15, value = 4},
    Item {iname = Lamp, weight= 25, value = 5},
    Item {iname = Box, weight= 20, value = 6},
    Item {iname = Rope, weight= 20, value = 7},
    Item {iname = Photoframe, weight= 17, value = 7},
    Item {iname = Dumbell, weight= 20, value = 8},
    Item {iname = Key, weight= 15, value = 1},
    Item {iname = Sword, weight= 20, value = 9},
    Item {iname = Pen, weight= 16, value = 10}]

