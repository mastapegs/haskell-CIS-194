module Main where

import qualified CourseOne (doubleEveryOther, toDigits, validate)
import CourseTwo.LogAnalysis (printAllLinesInTree)

main :: IO ()
main = do
  printAllLinesInTree
