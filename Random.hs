module Random (
    seed,
    range_random
) where

import Data.Time.Clock

seed::IO Int
seed = do 
        x <- fmap show getCurrentTime
        let y = read $ take 6 $ drop 20 x
        return y

range_random:: Int -> Int -> Int --Seed Max
range_random s m = mod s m

