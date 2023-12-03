module Day2 where

import Prelude
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String (split, replaceAll, contains, trim, length, take)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Array (foldl, takeEnd, filter, head, nub, find, sort, reverse)
import Data.Maybe (Maybe(..), fromMaybe)

type Game
  = { game :: Int
    , cubes :: Array String
    , impossible :: Boolean
    , power :: Int
    }

mapImpossible :: String -> Boolean
mapImpossible input = do
  if contains (Pattern "R") input && input >= "13R" then
    true
  else if contains (Pattern "G") input && input >= "14G" then
    true
  else if contains (Pattern "B") input && input >= "15B" then
    true
  else
    false

toNum :: String -> String
toNum input = case length input of
  2 -> "0" <> input
  _ -> input

filterMaybe :: Maybe String -> Boolean
filterMaybe maybeStr = case maybeStr of
  Just _ -> true
  Nothing -> false

gameNumber :: Array String -> Game
gameNumber games = do
  let
    game =
      head games
        # fromMaybe ""
        # replaceAll (Pattern "Game ") (Replacement "")
        # fromString
        # fromMaybe 0

    cubes =
      takeEnd 1 games
        # head
        # fromMaybe ""
        # replaceAll (Pattern ";") (Replacement ",")
        # replaceAll (Pattern " red") (Replacement "R")
        # replaceAll (Pattern " green") (Replacement "G")
        # replaceAll (Pattern " blue") (Replacement "B")
        # split (Pattern ",")
        # map trim
        # map toNum
        # nub
        # sort
        # reverse

    impossibleGame = find mapImpossible cubes

    power :: Int
    power =
      foldl findMax [] cubes
        # map (take 2)
        # map fromString
        # map (fromMaybe 0)
        # foldl (*) 1
  { game: game, cubes: cubes, impossible: filterMaybe impossibleGame, power: power }

findMax :: Array String -> String -> Array String
findMax acc input = do
  if contains (Pattern "R") input && not (find (contains $ Pattern "R") acc # filterMaybe) then
    acc <> [ input ]
  else if contains (Pattern "G") input && not (find (contains $ Pattern "G") acc # filterMaybe) then
    acc <> [ input ]
  else if contains (Pattern "B") input && not (find (contains $ Pattern "B") acc # filterMaybe) then
    acc <> [ input ]
  else
    acc

day2 :: String -> Aff Unit
day2 input = do
  let
    result =
      split (Pattern "\n") input
        # map (split (Pattern ":"))
        # map gameNumber
        # filter (\line -> line.game /= 0)
        # map (\line -> line.power)
        --        # filter (\line -> not line.impossible)
        --        # map (\line -> line.game)
        # foldl (+) 0
  log $ show result

