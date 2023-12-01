module Day1 where

import Prelude
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String (split, replaceAll)
import Data.Array (foldl, takeEnd, filter, head)
import Data.Int (decimal, fromString, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)

filterMaybe :: Maybe Int -> Boolean
filterMaybe maybeInt = case maybeInt of
  Just _ -> true
  Nothing -> false

toInt :: Maybe Int -> Int
toInt maybeInt = case maybeInt of
  Just i -> i
  Nothing -> 0

firstAndLast :: Array Int -> Array Int
firstAndLast input = do
  let
    first = head input # fromMaybe 0

    last = takeEnd 1 input # head # fromMaybe first

    both = toStringAs decimal first <> toStringAs decimal last
  [ fromString both # toInt ]

-- the most beautiful piece of code
theShameMap :: String -> String
theShameMap string =
  string
    # replaceAll (Pattern "oneight") (Replacement "18")
    # replaceAll (Pattern "oneightwo") (Replacement "182")
    # replaceAll (Pattern "oneighthree") (Replacement "183")
    # replaceAll (Pattern "fiveight") (Replacement "58")
    # replaceAll (Pattern "fiveightwo") (Replacement "582")
    # replaceAll (Pattern "fiveighthree") (Replacement "583")
    # replaceAll (Pattern "nineight") (Replacement "98")
    # replaceAll (Pattern "nineightwo") (Replacement "982")
    # replaceAll (Pattern "nineighthree") (Replacement "983")
    # replaceAll (Pattern "eightwo") (Replacement "82")
    # replaceAll (Pattern "eighthree") (Replacement "83")
    # replaceAll (Pattern "sevenine") (Replacement "79")
    # replaceAll (Pattern "sevenineight") (Replacement "798")
    # replaceAll (Pattern "sevenineight") (Replacement "798")
    # replaceAll (Pattern "twone") (Replacement "21")
    # replaceAll (Pattern "one") (Replacement "1")
    # replaceAll (Pattern "two") (Replacement "2")
    # replaceAll (Pattern "three") (Replacement "3")
    # replaceAll (Pattern "four") (Replacement "4")
    # replaceAll (Pattern "five") (Replacement "5")
    # replaceAll (Pattern "six") (Replacement "6")
    # replaceAll (Pattern "eight") (Replacement "8")
    # replaceAll (Pattern "seven") (Replacement "7")
    # replaceAll (Pattern "nine") (Replacement "9")

day1 :: String -> Aff Unit
day1 input = do
  let
    justNumbers :: Array (Array Int)
    justNumbers =
      split (Pattern "\n") input
        # map \line ->
            theShameMap line
              # split (Pattern "")
              # map fromString
              # filter filterMaybe
              # map toInt

    result :: Int
    result =
      justNumbers
        # map firstAndLast
        # map (foldl (+) 0)
        # foldl (+) 0
  log $ show result
