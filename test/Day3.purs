module Test.Day3 where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Day3 (NumberPos, SymbolPos, concatNumber, flatNumber, flatSymbol, inputToMatrix, isGear, isPart, mapParts, mapToGear, mapToNumber, mapToSymbol, sumParts)
import Data.Array (foldl)
import Data.Maybe (Maybe(..))

input1 :: String
input1 =
  """
467..114..
...%......
..35..633.
....../...
617@......
.....-.58.
..592.....
......755.
...$.=....
.664.598..
"""

symbols :: Array SymbolPos
symbols =
  [ { x: 3, y: 1 }
  , { x: 6, y: 3 }
  , { x: 3, y: 4 }
  , { x: 5, y: 5 }
  , { x: 3, y: 8 }
  , { x: 5, y: 8 }
  ]

input2 :: String
input2 =
  """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

gears :: Array SymbolPos
gears = [{ x: 3, y: 1 },{ x: 3, y: 4 },{ x: 5, y: 8 }]

numbers :: Array NumberPos
numbers =
  [ { num: "467", xl: 2, xs: 0, y: 0 }
  , { num: "114", xl: 7, xs: 5, y: 0 }
  , { num: "35", xl: 3, xs: 2, y: 2 }
  , { num: "633", xl: 8, xs: 6, y: 2 }
  , { num: "617", xl: 2, xs: 0, y: 4 }
  , { num: "58", xl: 8, xs: 7, y: 5 }
  , { num: "592", xl: 4, xs: 2, y: 6 }
  , { num: "755", xl: 8, xs: 6, y: 7 }
  , { num: "664", xl: 3, xs: 1, y: 9 }
  , { num: "598", xl: 7, xs: 5, y: 9 }
  ]

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "day3 part 1" do
          it "finds all symbols" do
            let
              allSymbols =
                inputToMatrix input1
                  # mapToSymbol
                  # flatSymbol
            allSymbols `shouldEqual` symbols
          it "finds all numbers" do
            let
              allNumbers =
                inputToMatrix input1
                  # mapToNumber
                  # flatNumber
                  # foldl concatNumber []
            allNumbers `shouldEqual` numbers
          it "check if is part" do
            let
                symbol = { x: 3, y: 1 }
                number = { num: "7", xl: 2, xs: 2, y: 0 }

                part = isPart number symbol
            part `shouldEqual` Just number
          it "maps all" do
            let
                allSymbols =
                    inputToMatrix input1
                        # mapToSymbol
                        # flatSymbol
                allNumbers =
                    inputToMatrix input1
                      # mapToNumber
                      # flatNumber
                      # foldl concatNumber []
                all = mapParts allNumbers allSymbols
                sum = sumParts all
            sum `shouldEqual` 4361
        describe "day3 part 1" do
          it "finds all gears" do
            let
              allGears =
                inputToMatrix input2
                  # mapToGear
                  # flatSymbol
            allGears `shouldEqual` gears
          it "check if is gear" do
            let
                symbol = { x: 3, y: 1 }
                numbers = [{ num: "7", xl: 2, xs: 2, y: 0 }]

                gear = isGear symbol numbers
            gear `shouldEqual` [Just numbers]

