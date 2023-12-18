module Day3 where

import Prelude
import Data.Array (concat, dropEnd, filter, foldl, head, mapWithIndex, takeEnd)
import Data.String.Pattern (Pattern(..))
import Data.String (split, null, length)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff (Aff)
import Effect.Class.Console (log)

inputToMatrix :: String -> Array (Array String)
inputToMatrix input =
  split (Pattern "\n") input
    # filter (not null)
    # map (split (Pattern ""))

type SymbolPos
  = { x :: Int
    , y :: Int
    }

maybeSymbol :: String -> Int -> Int -> Maybe SymbolPos
maybeSymbol input x y = case fromString input of
  Just _ -> Nothing
  Nothing -> do
    if input == "." then
      Nothing
    else
      Just { x: x, y: y }

mapToSymbol :: Array (Array String) -> Array (Array (Maybe SymbolPos))
mapToSymbol input =
  input
    # mapWithIndex \y row ->
        row
          # mapWithIndex \x symbol -> maybeSymbol symbol x y

flatSymbol :: Array (Array (Maybe SymbolPos)) -> Array SymbolPos
flatSymbol input =
  input # concat # filter isJust
    # map (fromMaybe { x: 0, y: 0 })

type NumberPos
  = { xs :: Int
    , xl :: Int
    , y :: Int
    , num :: String
    }

maybeNumber :: String -> Int -> Int -> Maybe NumberPos
maybeNumber input x y = case fromString input of
  Just _ -> Just { xs: x, xl: x + (length input) - 1, y: y, num: input }
  Nothing -> Nothing

mapToNumber :: Array (Array String) -> Array (Array (Maybe NumberPos))
mapToNumber input =
  input
    # mapWithIndex \y row ->
        row
          # mapWithIndex \x item -> maybeNumber item x y

flatNumber :: Array (Array (Maybe NumberPos)) -> Array NumberPos
flatNumber input =
  input # concat # filter isJust
    # map (fromMaybe { xs: 0, xl: 0, y: 0, num: "0" })

concatNumber :: Array NumberPos -> NumberPos -> Array NumberPos
concatNumber acc input = case takeEnd 1 acc # head of
  Just last -> do
    if (last.xs == (input.xs - 1) || last.xl == (input.xs - 1)) && (last.y == input.y) then
      (dropEnd 1 acc)
        <> [ { xs: last.xs
            , xl: input.xs
            , y: last.y
            , num: last.num <> input.num
            }
          ]
    else
      acc <> [ input ]
  Nothing -> [ input ]

isPart :: NumberPos -> SymbolPos -> Maybe NumberPos
isPart number symbol =
    if (symbol.x >= number.xs - 1 && symbol.x <= number.xl + 1)
        && (symbol.y >= number.y - 1 && symbol.y <= number.y + 1) then
        Just number
    else
        Nothing

mapParts :: Array NumberPos -> Array SymbolPos -> Array NumberPos
mapParts numbers symbols =
    numbers
        # map (\number -> (symbols # map (\symbol -> isPart number symbol)))
        # concat
        # filter isJust
        # map (fromMaybe { xs: 0, xl: 0, y: 0, num: "0" })

sumParts :: Array NumberPos -> Int
sumParts numbers =
    numbers # map (\n -> (n.num # fromString # fromMaybe 0)) # foldl (+) 0

maybeGear :: String -> Int -> Int -> Maybe SymbolPos
maybeGear input x y =
    if input == "*" then
      Just { x: x, y: y }
    else
      Nothing

mapToGear :: Array (Array String) -> Array (Array (Maybe SymbolPos))
mapToGear input =
  input
    # mapWithIndex \y row ->
        row
          # mapWithIndex \x symbol -> maybeGear symbol x y

isGear :: SymbolPos -> Array NumberPos -> Array (Maybe NumberPos)
isGear gear numbers  =
    if (symbol.x >= number.xs - 1 && symbol.x <= number.xl + 1)
        && (symbol.y >= number.y - 1 && symbol.y <= number.y + 1) then
        Just number
    else
        Nothing

day3 :: String -> Aff Unit
day3 input = do
  let
    allSymbols =
        inputToMatrix input
            # mapToSymbol
            # flatSymbol
    allNumbers =
        inputToMatrix input
          # mapToNumber
          # flatNumber
          # foldl concatNumber []
    part1 = mapParts allNumbers allSymbols # sumParts
  log $ show part1


-- 532445