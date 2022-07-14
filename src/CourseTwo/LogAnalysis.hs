{-# OPTIONS_GHC -Wall #-}

module CourseTwo.LogAnalysis where

import CourseTwo.Log
  ( LogMessage (..),
    MessageType (Error, Info),
    testParse,
  )
import Data.Foldable (for_)

parse :: String -> [LogMessage]
parse rawString = map parseMessage $ lines rawString

parseMessage :: String -> LogMessage
parseMessage string = case words string of
  "I" : timestamp : message ->
    LogMessage Info (read timestamp) (unwords message)
  "E" : errorLevel : timestamp : message ->
    LogMessage (Error (read errorLevel)) (read timestamp) (unwords message)
  line -> Unknown $ show line

runParserWithLines :: Int -> IO [LogMessage]
runParserWithLines n = testParse parse n "src/CourseTwo/error.log"

printResultsWithLines :: Int -> IO ()
printResultsWithLines n = do
  logMessages <- runParserWithLines n
  for_ logMessages print

printLines :: Int -> IO ()
printLines = printResultsWithLines