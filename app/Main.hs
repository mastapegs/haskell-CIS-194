module Main where

import qualified CourseOne (doubleEveryOther, toDigits, validate)
import CourseTwo.LogAnalysis

main :: IO ()
main = do
  print (CourseOne.toDigits 123)
  print (CourseOne.toDigits (-17))
  print (CourseOne.toDigits 0)
  print ((CourseOne.doubleEveryOther . CourseOne.toDigits) 1234)
  print ((CourseOne.doubleEveryOther . CourseOne.toDigits) 70845)
  print ((CourseOne.doubleEveryOther . CourseOne.toDigits) 4389)
  print $ CourseOne.validate 4012888888881881
  print $ CourseOne.validate 4012888888881882
