module Command where

import Text.Parsec hiding
  ( parse
  , choice
  , (<|>)
  , sepBy
  , sepBy1
  , many
  , many1
  )
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Item
import Direction

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

choice :: [Parser a] -> Parser a
choice = P.choice . map try

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy body sep = P.sepBy1 body (P.try sep)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 body sep = P.sepBy1 body (P.try sep)

many :: Parser a -> Parser [a]
many = P.many . try

many1 :: Parser a -> Parser [a]
many1 = P.many1 . try

parse :: Parser a -> String -> Either ParseError a 
parse prsr = P.parse prsr ""


data Command
  = Inventory
  | Steps
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Unlock
  | Exit
  | Poke
  deriving (Eq, Show)

type Conjunction = [Command]

itemNameP :: Parser ItemName

itemNameP = choice $ map (\itm -> pure itm <* string (show itm)) itemNames

nounPhrase_stub :: Parser [ItemName]

nounPhrase_stub = do
    itm <- itemNameP
    pure [itm]

nounPhrase :: Parser [ItemName]

nounPhrase = sepBy1 itemNameP (string "," <|> string ", ")


inventoryP :: Parser Command

inventoryP = pure Inventory <* string ("inventory")

inventoryP_monadically :: Parser Command
inventoryP_monadically = do
    _<- string "inventory"
    pure Inventory

stepsP :: Parser Command

stepsP = pure Steps <* string ("steps")

takeP :: Parser Command

takeP = do
    _ <- string "take "
    itm <- nounPhrase
    pure (Take itm)

exitP :: Parser Command

exitP = pure Exit <* choice (map string ["quit", "exit", "this is just a dream"])

lookP :: Parser Command

lookP = do
    _<- string "look"
    pure Look

dropP :: Parser Command

dropP = do
    _ <- string "drop "
    itm <- nounPhrase
    pure (Drop itm)

directionP :: Parser Direction

directionP =  choice $ map (\dir -> pure dir <* string (show dir)) directions

pokeP :: Parser Command

pokeP = do
    _ <- string "poke"
    pure (Poke)

moveP :: Parser Command

moveP = do
    dir <- directionP
    pure (Move dir)

unlockP :: Parser Command

unlockP = do
    _<- string "unlock"
    pure Unlock

commandP :: Parser Command

commandP = moveP <|> dropP <|> takeP <|> exitP <|> inventoryP <|> lookP <|> unlockP <|> stepsP <|> pokeP

conjunctionP :: Parser Conjunction

conjunctionP = sepBy1 commandP (string " and ") <* eof

parseInput :: String -> Maybe Conjunction

parseInput str = case parse conjunctionP str of
    Left _ -> Nothing
    Right r -> Just r
