------------------------
-- # LogAnalysis.hs # --
------------------------

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

----------------------
-- ## Exercise 1 ## --
----------------------

-- | @parseMessage s@ parses string @s@ as a @LogMessage@.
parseMessage :: String -> LogMessage
parseMessage s@(c : ' ' : rest) = case c of
  'E' -> LogMessage (Error lvl) ts msg
    where
      lvl = read . head . words $ rest
      ts = read . head . tail . words $ rest
      msg = unwords . tail . tail . words $ rest
  'I' -> LogMessage Info ts1 msg1
  'W' -> LogMessage Warning ts1 msg1
  _ -> Unknown s
  where
    ts1 = read . head . words $ rest
    msg1 = unwords . tail . words $ rest
parseMessage s = Unknown s

-- | @parse s@ parses the string @s@ as a list of @LogMessage@s.
parse :: String -> [LogMessage]
parse s = map parseMessage . lines $ s

----------------------
-- ## Exercise 2 ## --
----------------------

-- | @insert lmsg msgt@ inserts @lmsg :: LogMessage@ to @msgt :: MessageTree@.
insert :: LogMessage -> MessageTree -> MessageTree
insert _ (Node _ (Unknown _) _) = Leaf -- we should never hit this case
insert (Unknown _) msgt = msgt
insert lmsg Leaf = Node Leaf lmsg Leaf
insert lmsg@(LogMessage _ ts1 _) (Node left center@(LogMessage _ ts2 _) right)
  = if ts1 < ts2
    then Node (insert lmsg left) center right
    else Node left center (insert lmsg right)

----------------------
-- ## Exercise 3 ## --
----------------------

-- | @build@ builds a @MessageTree@ from a @[LogMessage]@.
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x : xs) = insert x (build xs)

----------------------
-- ## Exercise 4 ## --
----------------------

-- | @inOrder@ creates a @[LogMessage]@ ordered by timestamp.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf c r) = c : inOrder r
inOrder (Node l c r) = inOrder l ++ inOrder (Node Leaf c r)

----------------------
-- ## Exercise 5 ## --
----------------------

-- | @whatWentWrong lmsgs@ takes a (potentially) unsorted @[LogMessage]@
--   and returns the @Error@s (in order) with severity at least 50.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lmsgs = map justMsg . filter hiSev . inOrder . build $ lmsgs
  where
    hiSev (LogMessage (Error lvl) _ _) = lvl >= 50
    hiSev _ = False
    justMsg (LogMessage _ _ msg) = msg
    justMsg _ = ""
