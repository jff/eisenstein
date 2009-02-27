module Eisenstein where

import Data.Maybe
import Data.Ratio

-- Newman's algorithm to list all positive rationals
newman :: [Rational]
newman = iterate nextCW $ 1%1
 where nextCW :: Rational -> Rational
       nextCW r  =  let  (n,m)  = (numerator r, denominator r)
                         j      = floor (n%m)
                    in   m%((2*j+1)*m-n)


-- Eisenstein function
ei :: Integer -> Integer -> [Integer]
ei m n = m : eiloop 1 1 m n m n
 where eiloop a 1 m n cm cn = n : cm : eiloop 1 (a+1) cm (a*cm+cn) cm cn
       eiloop a b m n cm cn = let k = 2*floor(a%b)+1
                              in  n: eiloop b (k*b-a) n (k*n-m) cm cn

-- Eisenstein function with a and b replaced by m and n in the calculation of k
ei' :: Integer -> Integer -> [Integer]
ei' m n = m : eiloop 1 1 m n m n
 where eiloop a 1 m n cm cn = n : cm : eiloop 1 (a+1) cm (a*cm+cn) cm cn
       eiloop a b m n cm cn = let k = 2*floor(m%n)+1
                              in  n: eiloop b (k*b-a) n (k*n-m) cm cn

extnewman :: Integer -> Integer -> [Integer]
extnewman cm cn = cm: loop 0 cm cn cm cn
 where loop r m n cm cn | ((m==(cm+r*cn)) && (n==cn)) = n: cm: loop (r+1) cm ((r+1)*cm+cn) cm cn
                        | otherwise                   = let k = 2*floor(m%n)+1
                                                        in  n: loop r n (k*n-m) cm cn
