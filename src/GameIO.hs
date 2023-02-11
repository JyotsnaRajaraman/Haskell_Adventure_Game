module GameIO where

import Control.Monad.State
import System.Exit
import System.Random
import System.IO
import GameState
import Room
import Command
import Item

type GameIO a = StateT GameState IO a

effectChange :: (GameState -> GameState) -> GameIO ()

effectChange = modify

chooseGame :: [a] -> GameIO a
chooseGame lst = do
    index <- randomRIO (0,length lst -1)
    return (lst !! index)


prompt :: GameIO ()

prompt = lift $ putStr "->" >> hFlush stdout

printMessage :: GameIO ()

printMessage = do 
    gstate <- get
    case message gstate of 
        Just m -> lift $ putStrLn m
        Nothing -> lift $ pure ()
    put $ setMessage "" gstate

printDescription :: GameIO ()

printDescription = do
    gstate <- get
    let room = currentRoom gstate
    lift $ putStrLn (desc room)

printMoves :: GameIO ()

printMoves = do
    gstate <- get
    let number = moves gstate
    lift $ putStrLn ("You've made " ++ show number ++ " move(s).")

printObjects :: GameIO ()

printObjects = do 
    gstate <- get
    let objs = nearbyObjects gstate
    case objs of 
        [] -> pure ()
        (x:xs) -> do
            lift . putStrLn . foldl (\acc item -> acc <> "\n" <> show item) "you can see the following objects" $ objs

printExits :: GameIO ()

printExits = do 
    gstate <- get
    let roomexits = currentExits (rname (currentRoom gstate)) gstate
    case roomexits of
        [] -> pure ()
        (x:xs) -> do
            lift . putStrLn . foldl (\acc dir -> acc <> "\n" <> show dir)  "There are exits in the following directions:" $ roomexits

printInventory :: GameIO ()

printInventory = do
    gstate <- get
    let objs = currentInventory gstate
    case objs of
        [] -> lift $ putStrLn "You are not carrying anything"
        (x:xs) -> do
            lift . putStrLn . foldl (\acc item -> acc <> "\n" <> show item) "you are carrying the following objects:" $ objs

actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()

actionOverList action items = do
    gs <- get    
    case items of
        [] -> pure()
        (x:xs) -> (effectChange (action x)) >> printMessage >> actionOverList action xs

  
finishGame :: GameIO ()

finishGame =  lift $ putStrLn ("Looks like you have a needle so you can poke yourself and wake up, PHEW that was just a bad dream!!") >>exitSuccess>>(return ())

exit :: GameIO ()

exit = lift $ putStrLn ("Don't worry, you're awake now and the monster can't get you (..until next time)") >>exitSuccess>>(return ())

poke :: GameIO ()

poke = do
    gstate <- get
    let obj = head (currentInventory gstate ++ [NothingThere])
    let currentr = currentRoom gstate
    case obj == Needle || rname currentr == rname bedroom of
        True -> case obj == Needle of
                    True -> case rname currentr == rname bedroom of
                         True -> lift $ putStrLn ("You poked yourself and you're up now!! The monster can't get you (..until next time)") >>exitSuccess>>(return ())
                         False -> lift $ putStrLn ("Where are you poking? Go find yourself")
                    False ->  lift $ putStrLn ("You don't have a sharp object to poke yourself with")
        False -> lift $ putStrLn ("You need to be in the bedroom and have a sharp object to poke yourself with")


checkGameOver :: GameIO ()

checkGameOver = do
    gstate <- get
    mm <- monsterMoveCheck gstate
    let obj = head (currentInventory gstate ++ [NothingThere])
    case (moves gstate) > 10 of
        False -> pure()
        True -> case mm of 
                    True ->  lift (putStrLn ("Phewww you had " ++  show obj ++ " monster is dead!")) >> printMessage >> lift exitSuccess>>(return ())
                    False -> lift (putStrLn ("Uh oh..you had " ++ show obj ++ ", but the ")) >>printMessage>> effectChange reset >> printMessage

                               

monsterMoveCheck ::  GameState -> GameIO Bool

monsterMoveCheck gstate = do
    monsteritem <- chooseGame monsterItemList
    let playeritem = getObjectUniv (head (currentInventory gstate ++ [NothingThere])) univ
    effectChange (setMessage ("monster had " ++ show (iname monsteritem))) 
    case (value monsteritem) < (value playeritem) of
        True -> return True
        False -> return False


syntaxError :: GameIO ()

syntaxError = lift $ putStrLn ("I don't understand that.")

opening :: GameIO ()

opening = lift $ putStrLn ("Welcome to Functional Adventure!")

performCommand :: Command -> GameIO ()

performCommand cmd = case cmd of
    Look -> printDescription >> printObjects >> printExits
    Inventory -> printInventory
    Take its -> actionOverList takeItem its
    Drop its -> actionOverList dropItem its
    Exit -> exit
    Steps -> printMoves
    Poke -> poke
    Unlock -> effectChange (unlock) >> printMessage >> printDescription
    Move dirs -> effectChange (move dirs) >> printMessage

performConjunction :: Conjunction -> GameIO ()

performConjunction conj = case conj of
    [] -> pure()
    (x:xs) -> performCommand x >> performConjunction xs


parseConjunction :: String -> GameIO ()

parseConjunction str = case parseInput str of
    Nothing -> syntaxError
    Just conj -> performConjunction conj

repl :: GameIO ()

repl  = do
    prompt
    line <- lift $ getLine
    parseConjunction line
    checkGameOver
