module Debug (table, trace') where

import qualified Debug.Trace as Trace
import Data.List

print_ x =  putStr $ (show x) ++ "\t" 

table xxs 
    | length (nub [length xs | xs <- xxs])/=1 = error "not simetric"
        | otherwise = mapM_ printRow xxs 
                where printRow xs =  (mapM_ print_) xs >> putStrLn "" 


trace' :: Show a => [Char] -> a -> a
trace' name x = Trace.trace (name ++ ": " ++ show x) x


