{- Assignment 1
 - Name: David Maher Messiha
 - Date: September 27, 2019
 -}
module Assign_1 where

macid :: String
macid = "mahermed"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: Evaluates Q with 3 inputs of type double and then returns an output of type double.
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3*a*c) - (b^2)) / (9*a^2)
{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: Evaluates R with 4 inputs of type double and then returns an output of type double.
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9*a*b*c) - (27*a^2*d) - (2*b^3)) / (54*a^3)
{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: Evaluates the discriminant with 2 inputs of type double and then returns an output of type double.
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = (q^3 + r^2)
{- -----------------------------------------------------------------
 - cqrt
 - -----------------------------------------------------------------
 - Description: cqrt evaluates the cube root of both positive and negative numbers.
 -}
cqrt :: Double -> Double
cqrt x = 
    if x < 0 then (-(-x)**(1/3))        -- If a negative number is inputed into cqrt, then the function evaluates the input by making it negative, evaluating, then making it negative again.
    else (x**(1/3))                     -- If the statement above is false, then the function evaluates the cube root by taking the inputs 1/3 power.
{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: Evaluates S with 2 inputs of type double and then returns an output of type double. 
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cqrt (r + (sqrt ((q^3) + (r^2))))
{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: Evaluates T with 2 inputs of type double and then returns an output of type double.
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cqrt (r - (sqrt ((q^3) + (r^2))))
{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: Evaluates the real Solutions of the cubic equation with 4 inputs of type double and then by returning an output of type double contained in a list. 
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d = s where
    sr = cubicR a b c d                                                         -- Define R in terms of a b c 
    sq = cubicQ a b c                                                           -- Define Q in terms of a b c d
    ss = cubicS sq sr                                                           -- Define S in terms of Q and R defined above
    st = cubicT sq sr                                                           -- Define T in terms of Q and R defined above
    x1 = ss + st - (b / (3*a))                                                  -- Evaluate x1 and x2 in terms of a, b, S, and T
    x2 = ((-(ss + st)) / 2) - (b / (3*a)) + (((sqrt 3) / 2) * (ss - st))
    solution :: Double -> [Double]                                              -- Create a variable "Solution" that takes an input of type double and returns an output of type double contained in a list
    cd = cubicDisc sq sr                                                        -- Define a variable "cd" which is equal to the cubicDisc function with inputs Q and R
    s1 = [x1]
    s2 = [x1,x2,x2]
    s3 = []
    s = solution cd                                                             -- Define variable "s" which is equal to the real solutions in terms of "cd"
    solution cd =
        if cd > 0 
            then s1                                                             -- A series of conditions that returns the correct solution by evaluating "cd" which is the discriminant
            else if cd == 0
                then s2
                else if cd < 0
                    then s3
                    else error "NaN"