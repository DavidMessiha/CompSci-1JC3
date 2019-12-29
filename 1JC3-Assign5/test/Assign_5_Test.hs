{- Assignment 5 Tests
 - Name:David Maher Messiha
 - Date: November 29, 2019
 -}

import Assign_5

import Test.QuickCheck

main :: IO ()
main = do print "Performing Test 1: "
          quickCheck funHProp
          do print "Performing Test 2: "
          quickCheck funKProp
          do print "Performing Test 3: "
          quickCheck definiteIntegralProp



funHProp :: Integer -> Bool
funHProp n = funH (abs n) >= 0

funKProp :: Double -> Bool
funKProp n = round (funK (abs n)) == round (definiteIntegral (-1) 1 (\x -> (abs n)**(-x)) 1000) 

definiteIntegralProp :: Double -> Double -> Integer -> Bool
definiteIntegralProp a b n = round (abs (definiteIntegral a b (\x -> x^2) n)) == round (abs (definiteIntegral b a (\x -> x^2) n))

{-
QuickCheck 
 -------------------------------------------------------------------
 Function: definiteIntegralProp
 Property: The absolute value of a definite integral from [a,b] is the same as the absolute value of the definite integral from [b,a]
 Actual Test Result:Pass
 -------------------------------------------------------------------
 Function: funHProp
 Property: For any n >= 0 the definite integral inbetween the curves x^n and x^1/n is >= to 0
 Actual Test Result:Pass
 -------------------------------------------------------------------
 Function: funKProp
 Property: The definite integral of a^x from [-1,1] is equal to the definite integral of a^-x from [-1,1] for any a
 Actual Test Result:Pass
 -------------------------------------------------------------------
 -}