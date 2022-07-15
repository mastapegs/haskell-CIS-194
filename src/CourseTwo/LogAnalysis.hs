{-# OPTIONS_GHC -Wall #-}

module CourseTwo.LogAnalysis where

import CourseTwo.Deps.Log
  ( LogMessage (..),
    MessageTree (..),
    MessageType (Error, Info, Warning),
    testParse,
  )
import Data.Foldable (for_)

-- Parse fns

parse :: String -> [LogMessage]
parse rawString = map parseMessage $ lines rawString

parseMessage :: String -> LogMessage
parseMessage string = case words string of
  "I" : timestamp : message ->
    LogMessage Info (read timestamp) (unwords message)
  "W" : timestamp : message ->
    LogMessage Warning (read timestamp) (unwords message)
  "E" : errorLevel : timestamp : message ->
    LogMessage (Error (read errorLevel)) (read timestamp) (unwords message)
  line -> Unknown $ show line

runParserWithLines :: Int -> IO [LogMessage]
runParserWithLines n = testParse parse n "src/CourseTwo/Deps/error.log"

runParser :: IO [LogMessage]
runParser = runParserWithLines 6000

-- Tree fns

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert _ tree@(Node _ (Unknown _) _) = tree
insert
  logMessage@(LogMessage _ stampInInsert _)
  (Node leftTree messageNode@(LogMessage _ stampInTree _) rightTree) =
    if stampInInsert <= stampInTree
      then Node (insert logMessage leftTree) messageNode rightTree
      else Node leftTree messageNode (insert logMessage rightTree)

build :: [LogMessage] -> MessageTree
build logMessages = buildHelper logMessages Leaf
  where
    buildHelper :: [LogMessage] -> MessageTree -> MessageTree
    buildHelper [] tree = tree
    buildHelper (x : xs) tree = buildHelper xs (insert x tree)

-- print fns

printAllLinesInTree :: IO ()
printAllLinesInTree = do
  logMessages <- runParser
  print $ build logMessages

printLines :: Int -> IO ()
printLines n = do
  logMessages <- runParserWithLines n
  for_ logMessages print

printAllLines :: IO ()
printAllLines = printLines 6000