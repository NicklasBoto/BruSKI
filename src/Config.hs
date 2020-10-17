module Config (preludePath, arityBlock, versionString) where

import Paths_BruSKI (version)
import Data.Version (showVersion)

-- Show the current version
versionString = showVersion version ++ " - October 2020"

-- Block arities higher than the number of binders
arityBlock = False

-- Path to prelude
preludePath = "/home/nicbot/Programming/BruSKI/src/Prelude/"

