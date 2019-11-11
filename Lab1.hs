import Test.QuickCheck
{- Lab 1, 2019
   Authors: Adam Rohdell, Amanda Dehlén
   Lab group: 48
 -}
---------------------------------------------
power :: Int -> Int -> Int
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower n k = k + 1

-- B -------------------------
-- power2
power2 n k
   | k < 0 = error "power: negative argument"
power2 n k = product(replicate k n)

-- C -------------------------
-- power3
power3 n k
   | k < 0 = error "power: negative argument"
power3 n 0 = 1
power3 n k 
    | odd k = n * power3 n (k-1)
    | otherwise = power3 (n*n) (div k 2) 

-- D -------------------------
{- 
 We should always test when k == 0 and we should test both even
 and odd numbers for power3, since it uses different calculations depending if k is even or not.
 We can test large numbers to see if power3 is faster than power2 as is intended.

    n = [2,10,3,5,7,8,24,100,50,700]
    k = [0,1,2,3,5,6,10,100,200,250]

 -}
ns :: [Int]
ns = [2,10,3,5,7,8,24,100,50,700]

ks :: [Int]
ks = [0,1,2,3,5,6,10,100,200,250]

-- 
prop_powers :: Int -> Int -> Bool
prop_powers n k = power n k == power2 n k && power2 n k == power3 n k

--
powerTest :: [Int] -> [Int] -> Bool
powerTest [n] [k] = prop_powers n k
powerTest (n:ns) (k:ks) = (prop_powers n k) && (powerTest ns ks)


    

--
prop_powers' :: Int -> Int -> Bool
prop_powers' n k 
 | k < 0 = prop_powers' n (abs k)
prop_powers' = power n k == power2 n k && power2 n k == power3 n k


