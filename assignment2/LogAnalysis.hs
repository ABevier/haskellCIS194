{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parse :: String -> [LogMessage]
parse n = mapToLogMessages(lines(n))


mapToLogMessages :: [String] -> [LogMessage]
mapToLogMessages [] = []
mapToLogMessages (x:xs) = parseMessage(x) : mapToLogMessages(xs)


parseMessage :: String -> LogMessage
parseMessage x = makeLogMessage(words x) 


makeLogMessage :: ([String] -> LogMessage)
makeLogMessage ("I":ts:xs) = LogMessage Info (read ts) (unwords xs)
makeLogMessage ("E":sv:ts:xs) = LogMessage (Error (read sv)) (read ts) (unwords xs)
makeLogMessage ("W":ts:xs) = LogMessage Warning (read ts) (unwords xs)
makeLogMessage xs = Unknown (unwords xs)


build :: [LogMessage] -> MessageTree
build [] = Leaf
build ((Unknown _):xs) = build xs
build (x:xs) = build' xs (Node Leaf x Leaf)


build' :: [LogMessage] -> MessageTree -> MessageTree
build' [] tree = tree
build' (x:xs) tree = build' xs (insert x tree)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) n = n
insert msg (Leaf) = Node Leaf msg Leaf
insert msg (Node left value right) = case (compareLog msg value) of
                                        True -> (Node left value (insert msg right))
                                        False -> (Node (insert msg left) value right)


compareLog :: LogMessage -> LogMessage -> Bool
compareLog (LogMessage _ ts1 _) (LogMessage _ ts2 _) = ts1 > ts2
compareLog _ _ = False


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left value right) = (inOrder left) ++ value : (inOrder right)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = doFilter $ inOrder(build messages)


doFilter :: [LogMessage] -> [String]
doFilter [] = []
doFilter (x:xs) = case x of
                       (LogMessage (Error n) _ message) | (n >= 50) -> message : (doFilter xs)
                       _ -> doFilter xs




testUtil :: IO ()
testUtil = do
          messages <- testParse parse 10 "error.log"
          putStrLn $ show $ inOrder(build messages)


main :: IO ()
main = print("Hello")