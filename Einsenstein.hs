module Main where

import EnumeratingRationals
import Math.OEIS
import Data.Maybe

numElems :: Int
numElems = 20

list2string :: (Show a) => [a] -> String
list2string = init . tail . show

oeis' :: Integer -> Integer -> IO ()
oeis' m n = do l <- lookupOEIS . list2string . (take numElems) $ ei m n
               putStrLn $ "Ei(" ++ show m ++ "," ++ show n ++ "):\n\t" ++ (head l)

oeis :: Integer -> Integer -> IO ()
oeis m n = do s <- searchSequence_IO . list2string . (take numElems) $ ei m n
              r <- getDataSeq s
              putStrLn $ "Ei(" ++ show m ++ "," ++ show n ++ "):\n\t" ++ r

getDataSeq :: (Maybe OEISSequence) -> (IO String)
getDataSeq Nothing    = return "Sequence not found."
getDataSeq (Just seq) = return $ (description seq) ++ " ( " ++ (concatMap (++ " ") (catalogNums seq)) ++ ")"

main :: IO ()
main = sequence_ [oeis a b | a <- [0..100], b <- [0..100]]
