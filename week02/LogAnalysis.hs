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
parseMessage s@(c : rest) = LogMessage kind timestamp message
  where
    kind = case c of
      'I' -> Info
      'W' -> Warning
      'E' -> Error lvl
    lvl =

