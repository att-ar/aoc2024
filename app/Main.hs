module Main (main) where

import Day01.Day01 (day01P1, day01P2)

main :: IO ()
main = do
  print "--- Day 01 ---"
  print " -- Part 1"
  day01P1 "src/Day01/day01_small.txt"
  day01P1 "src/Day01/day01.txt"
  print " -- Part 2"
  day01P2 "src/Day01/day01_small.txt"
  day01P2 "src/Day01/day01.txt"
