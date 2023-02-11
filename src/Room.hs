module Room where

import Item
import Direction
import Data.List

data RoomName
  = Hall
  | Attic
  | Basement
  | Bathroom
  | Bedroom
  | Outside
  deriving (Eq, Ord)

instance Show RoomName where
    show anything = case anything of
        Hall -> "hall"
        Basement -> "basement"
        Attic -> "attic"
        Bathroom -> "bathroom"
        Bedroom -> "bedroom"
        Outside -> "outside"

type Exit = (Direction, RoomName)

data Room = Room { rname :: RoomName,
                    desc :: String,
                    exits :: [Exit],
                    objects :: [ItemName] 
                }
        deriving (Show, Eq)


hall = Room Hall "You're in the hall, you can go to the basement, attic or bathroom from here" [(N, Bathroom),(E, Attic),(S, Basement)] [Couch, Table, Pen]
attic = Room Attic "This is the attic, may be haunted - be careful what you pick up" [(W,Hall)] [Chest, Watch, Photoframe, Sword]
basement = Room Basement "You're in the basement, its kinda chilly - get out quick" [(N, Hall)] [Box, Dumbell, Rope, Needle]
bathroom = Room Bathroom "You're in the bathroom, you can go to the hall or bedroom" [(N, Bedroom),(S, Hall)] [Toothbrush, Toothpaste, Floss]
bedroom = Room Bedroom "Oh there you are sleeping in the bedroom?" [(S, Bathroom)] [Bed, Lamp]
outside = Room Outside "Quick!! You have ten moves before the monster catches you, use the key to open the door and get into the house!!Wake yourself up, or find a weapon to fight the monster" [] [Key]


rooms = [hall, attic, basement, bedroom, bathroom]

roomNames :: [RoomName]

roomNames = map rname rooms

addItem :: ItemName -> Room -> Room

addItem name Room {rname, desc, exits, objects} = Room {rname, desc, exits, objects = objects <> [name]}


removeItem :: ItemName -> Room -> Room

removeItem name  Room {rname, desc, exits, objects} = Room {rname, desc, exits, objects = delete name objects}

allRooms :: [Room]

allRooms = [hall, attic, basement, bedroom, bathroom, outside]


hasObjects :: Room -> Bool

hasObjects room = not (objects room == [])

