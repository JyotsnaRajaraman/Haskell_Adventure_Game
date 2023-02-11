module GameState where

--import Data.List
import Control.Exception
import qualified Data.Map as M
import System.Random
import Item
import Room
import Player
import Direction

choose :: [a] -> IO a
choose lst = do
    index <- randomRIO (0,length lst -1)
    return (lst !! index)


data KeyError = KeyError
  deriving Show

instance Exception KeyError

type Error a = Either String a

getObjectUniv :: ItemName -> Universe -> Item
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
getObject iname st = getObjectUniv iname (universe st)


getRoomMap :: RoomName -> GameMap -> Room
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
getRoom name st = getRoomMap name (gmap st)


type GameMap = M.Map RoomName Room


data GameState = GameState { message :: Maybe String,
                    gmap :: GameMap,
                    universe :: Universe,
                    player :: Player,
                    moves :: Integer
                }
        deriving (Show)

mkMap :: [Room] -> GameMap

makeIntoMap :: [Room] -> [(RoomName, Room)]

makeIntoMap list = map generateTuple list
    where generateTuple (Room rname desc exits objects) = (rname, Room rname desc exits objects)


mkMap lst = M.fromList (makeIntoMap lst)


gameMap :: GameMap

gameMap = mkMap allRooms


initialState = GameState {message = Nothing, gmap = gameMap, universe = univ, player = you, moves = 0}


setRoomMap :: RoomName -> Room -> GameMap -> GameMap
setRoomMap rold room mp = M.insert rold room mp

setMessage :: String -> GameState -> GameState

setMessage str GameState {message, gmap, universe, player, moves} = case str of
    "" ->  GameState {message = Nothing, gmap, universe, player, moves}
    _ -> GameState {message = pure(str), gmap, universe, player, moves}

steps :: GameState -> Integer

steps gs = moves gs
   
currentInventory :: GameState -> [ItemName]

currentInventory gs = inventory(player gs)

currentRoom :: GameState -> Room

currentRoom gs = getRoom (location(player gs)) gs


nearbyObjects :: GameState -> [ItemName]

nearbyObjects gstate = objects(currentRoom gstate)


actuallyTakeItem :: ItemName -> GameState -> GameState

actuallyTakeItem item gstate@GameState {message, gmap, universe, player, moves} = outputstate where 
    outputstate = setMessage ("you take " ++ show(item))  GameState {message, 
        gmap = setRoomMap (rname(currentRoom gstate)) (Room.removeItem item (currentRoom gstate)) gmap, 
        universe,
        player = Player.addItem item player, moves} 

    
actuallyDropItem :: ItemName -> GameState -> GameState
actuallyDropItem item gstate@GameState {message, gmap, universe, player, moves} = outputstate where
    outputstate = setMessage ("you dropped " ++ show(item))  GameState {message, 
        gmap = setRoomMap (rname(currentRoom gstate)) (Room.addItem item (currentRoom gstate)) gmap, 
        universe, 
        player = Player.removeItem item player, moves}


tmpWeight :: GameState -> ItemName -> Integer
tmpWeight gs item = weight $ getObjectUniv item (universe gs)


inventoryWeight :: GameState -> Integer
inventoryWeight gstate = 
    sum $ map (tmpWeight gstate) (currentInventory gstate)

inventoryContains :: ItemName -> GameState -> Bool

inventoryContains item gstate =  elem item (currentInventory(gstate))

alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState

alreadyHaveTakeCheck item gstate = case (inventoryContains item gstate) of 
    True -> Left ("You are already carrying " ++ show(item))
    False -> Right (gstate)

inRoomTakeCheck :: ItemName -> GameState -> Error GameState

inRoomTakeCheck item gstate = case elem item (nearbyObjects(gstate)) of
    True -> Right(gstate)
    False -> Left ("There is no " ++ show(item) ++ " here.")


weightCheck :: ItemName -> GameState -> Error GameState

weightCheck item gstate@GameState {message, gmap, universe, player, moves}  = 
    case (inventoryWeight(gstate) + tmpWeight gstate item) < maxWeight(player) of
        True -> Right(gstate)
        False -> Left ("You can't carry that, its either too heavy or you're already holding something")

anywhereDropCheck :: ItemName -> GameState -> Error GameState

anywhereDropCheck item gstate = 
    case (elem item (nearbyObjects(gstate)) || elem item (currentInventory(gstate))) of
        True -> Right(gstate)
        False -> Left ("What do you mean? Drop the " ++ show(item) ++ "??")


inRoomDropCheck :: ItemName -> GameState -> Error GameState

inRoomDropCheck item gstate =
    case (elem item (nearbyObjects(gstate))) of
        False -> Right(gstate)
        True -> Left ("You are not carrying the " ++ show(item))

takeItem :: ItemName -> GameState -> GameState

checkTakeItemFinal ::  ItemName -> GameState -> Error GameState

checkTakeItemFinal item gstate = ((alreadyHaveTakeCheck item gstate) >> (inRoomTakeCheck item gstate) >> (weightCheck item gstate))


takeItem item gstate@GameState {message, gmap, universe, player, moves} = 
    case checkTakeItemFinal item gstate of
        Left l -> (setMessage (l) gstate) 
        Right r -> actuallyTakeItem item gstate


dropItem :: ItemName -> GameState -> GameState

checkDropItemFinal ::  ItemName -> GameState -> Error GameState

checkDropItemFinal item gstate = (anywhereDropCheck item gstate) >> (inRoomDropCheck item gstate)


dropItem item gstate@GameState {message, gmap, universe, player, moves} =
    case checkDropItemFinal item gstate of
        Left l -> (setMessage (l) gstate)
        Right r -> actuallyDropItem item gstate


roomHasObjects :: GameState -> Bool

roomHasObjects gstate = hasObjects $ currentRoom gstate

currentExits :: RoomName -> GameState -> [Direction]

currentExits room gstate = fst $ unzip $ exits $ getRoom room gstate

destinationName :: Direction -> Room -> Maybe RoomName

destinationName dir room = lookup dir (exits(room)) 

updatedPlayerLocation :: Player -> RoomName -> Player

updatedPlayerLocation Player{inventory,maxWeight,location} rname = Player{inventory,maxWeight,location = rname}

updatedMoves :: GameState -> GameState

updatedMoves GameState {message, gmap, universe, player, moves} = GameState {message, gmap, universe, player, moves = moves + 1}

updatedPlayer :: Player -> GameState -> GameState

updatedPlayer newplayer GameState {message, gmap, universe, player, moves} =
    GameState {message, gmap, universe, player = newplayer, moves}

move :: Direction -> GameState -> GameState
move dir gstate@GameState {message, gmap, universe, player, moves} = 
    case (currentRoom gstate == outside) of
        True -> setMessage ("Use the key to unlock the house") gstate
        False -> case destinationName dir (currentRoom gstate) of
            Nothing -> setMessage ("No room there") gstate
            Just room -> setMessage ("you go " ++ show(dir)) (updatedPlayer (updatedPlayerLocation player room) (updatedMoves gstate))
                      
haveWonGame :: GameState -> Bool

haveWonGame gstate = 
    (rname (currentRoom gstate) == rname bedroom
    && inventoryContains Needle gstate)
    
unlock ::  GameState -> GameState
unlock gstate@GameState {message, gmap, universe, player, moves}= case (inventoryContains Key gstate) of
    False ->  setMessage ("How can you unlock the door without a key?") gstate
    True -> setMessage ("you're in the house, but the key got stuck in the door...No time to spare though!!!") (updatedPlayer (Player.removeItem Key (updatedPlayerLocation player Hall)) (updatedMoves gstate))


reset :: GameState -> GameState

reset gstate =  setMessage ("here we go again") initialState
