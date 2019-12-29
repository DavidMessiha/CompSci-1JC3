{- Assignment 5
 - Name: David Maher Messiha
 - Date: November 28, 2019
 -}
module Assign_5 where

macid :: String
macid = "mahermed"


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: Computes an aproximation of the area under the curve of any continuous function within the interval [a,b] using the trapezoidal rule
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n
    | (abs (g a) == abs (g b)) && (g a < 0) = 0.0           -- In the case that a symmetric function crosses the x axis with the same a and b value, the are should be 0
    | otherwise = (changex / 2 ) * foldr (\i acc -> (2 * ((g) ((a) + i * changex))) + acc) ((g (a)) + (g (b))) [1..((fromIntegral n)-1)]        --Implementation of the foldr function witht the trapezoidal formula
    where
        changex = ((b)-(a)) / (fromIntegral n)
{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description: Computes the area between the curves x^n and x^1/n for any n using the trapezoidal rule
 -}
funH :: Integer -> Double
funH n 
    | (n >= 0 && n <= 1) = (definiteIntegral 0 1 (\x -> x**(fromIntegral n)) 1000) - (definiteIntegral 0 1 (\x -> x**(1/(fromIntegral n))) 1000) --The graph of x^n is only greater than x^1/n for every n between 0 and 1, we swap whcih areas we subtract depending which function is greater
    | otherwise = (definiteIntegral 0 1 (\x -> x**(1/(fromIntegral n))) 1000) - (definiteIntegral 0 1 (\x -> x**(fromIntegral n)) 1000)
{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description: Compute the area betweeen the graph of x^n and the x axis between -1 and 1 for any n
 -}
funK :: Double -> Double
funK n = (definiteIntegral (-1) 1 (\x -> n**x) 1000) 

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:1
 - - Input: definiteIntegral (-2) (2) (\x -> x) 30
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:2
 - - Input: definiteIntegral (-2) (6) (\x -> x^2) 90
 - - Expected Output: 74.67720164609055
 - - Acutal Output: 74.67720164609055
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number:3
 - - Input: definiteIntegral (5) (20) (\x -> sin x) 90
 - - Expected Output: -0.12413173395139866
 - - Acutal Output: -0.12413173395139866
 - -----------------------------------------------------------------
 - - Function:funH
 - - Test Case Number:1
 - - Input: funH 1
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function:funH
 - - Test Case Number:2
 - - Input: funH 2
 - - Expected Output: 0.3333266343936821
 - - Acutal Output: 0.3333266343936821
 - -----------------------------------------------------------------
 - - Function:funH
 - - Test Case Number:3
 - - Input: funH 10000
 - - Expected Output: 0.9974243711207154
 - - Acutal Output: 0.9974243711207154
 - -----------------------------------------------------------------
 - - Function:funK
 - - Test Case Number:1
 - - Input: funK 1
 - - Expected Output:2.0
 - - Acutal Output: 2.0
 - -----------------------------------------------------------------
 - - Function:funK
 - - Test Case Number:2
 - - Input: funK 4
 - - Expected Output:2.70505493453454
 - - Acutal Output: 2.70505493453454
 - -----------------------------------------------------------------
 - - Function:funK
 - - Test Case Number:3
 - - Input: funK 1000000000000
 - - Expected Output: 3.6200416696887024e10
 - - Acutal Output: 3.6200416696887024e10
 - -----------------------------------------------------------------
 -}

