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

-- | @parse s@ parses the string @s@ as a list of *LogMessage@s.
parse :: String -> [LogMessage]
parse s = map parseMessage . lines $ s
