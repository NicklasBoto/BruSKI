module Unlambda.UserInputHandler
        ( uncomment
        , format
        ) where

import Data.Char

-----------------------------------------------------------------------
------------------------ User Input Handler ---------------------------
-----------------------------------------------------------------------

        {---*-_-*-_-*-_-*---
            Ugly?
            Yes!
        ---*-_-*-_-*-_-*---}

-- Filters away \n, \t, and " "
format :: String -> String
format = filter (not . isSpace) . uncomment

-- Removes full-line and in-line comments.
-- Note that Madore's Unlambda does not support inline commenting.
uncomment :: String -> String
uncomment []       = []
uncomment ('#':cs) = uncomment (dropUntil (\x -> x /= '\n' && x /= '#') cs)
uncomment (c  :cs) = c : uncomment cs

-- Like preludes dropWhile, but also drops the first element
-- where the predicate fails.
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] =  []
dropUntil p (x:xs)
            | p x = dropUntil p xs
            | otherwise =  xs

