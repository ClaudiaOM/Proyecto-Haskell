module Random where

import Data.Time.Clock

seed::IO String
seed = do 
        x <- fmap show getCurrentTime
        return $ take 7 $ drop 20 x
        

range_random:: Int -> Int -> Int --Seed Max
range_random s m = mod s m

