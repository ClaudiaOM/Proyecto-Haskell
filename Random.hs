module Random (
    seed,
    range_random
) where

import Data.Time.Clock
import Text.Read

seed::IO Int
seed = do 
        x <- fmap show getCurrentTime
        let y = take 6 $ drop 20 x
        case readMaybe y of
            Just z -> return z 
            Nothing -> seed

range_random:: Int -> Int -> Int --Seed Max
range_random s m = mod s m

