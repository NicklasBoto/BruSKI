module Config
        ( preludePath
        , arityBlock
        ) where


{-
Now I know that this is a poor way to do this...
But! i wanted to have this functionality in a jiffy, and such
decided to do it this way temporarily.
-}


-- Filepath to where BruSKI is installed
preludePath  = "/home/nicbot/Programming/BruSKI/src/Prelude/"

-- Whether or not to allow arity higher that the number of binders
arityBlock = False

