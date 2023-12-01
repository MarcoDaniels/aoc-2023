module Main where

import Prelude
import Effect (Effect)
import Fetch (Method(..), fetch)
import Effect.Class.Console (log)
import Effect.Aff (Aff, launchAff_)
import Data.Array (takeEnd)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Node.Process (argv)
import Day1 (day1)

session :: String
session = ""

fetchInput :: String -> Aff String
fetchInput day = do
  { text } <-
    fetch ("https://adventofcode.com/2023/day/" <> day <> "/input")
      { method: GET, headers: { "Cookie": "session=" <> session } }
  text

validateDay :: String -> Maybe String
validateDay day = case fromString day of
  Just _ -> Just day
  _ -> Nothing

fromInput :: Array String -> Maybe String
fromInput input = case takeEnd 1 input of
  [ day ] -> validateDay day
  [ day, _ ] -> validateDay day
  _ -> Nothing

main :: Effect Unit
main = do
  args <- argv
  case fromInput args of
    Just day ->
      launchAff_ do
        log $ show $ "fetching input for day: " <> day
        input <- fetchInput day
        case day of
          "1" -> day1 input
          _ -> log $ show $ "day" <> day <> " not implemented yet"
    Nothing -> log $ show $ "could not determine day from input" <> show args
