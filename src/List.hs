module List (hasDuplicates) where

import qualified Data.Set as Set

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
    where set = Set.fromList list
