{- Assignment 2
 - Name: David Maher Messiha
 - Date: October 16,2019
 -}
module Assign_2 where

macid :: String
macid = "mahermed"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description: This function returns the first part of a gaussion integer, the real part.
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (x,y) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description:  This function returns the second part of a gaussion integer, the imaginary part.
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (x,y) = y

{- -----------------------------------------------------------------
 - gausConj
 - -----------------------------------------------------------------
 - Description: This function returns the conjugate of any gaussion integer by making the imaginary part negative.
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj g = (gaussReal g,(-gaussImag g))

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description: This function returns the sum of two gaussion integers by implementing the formula: (a0+a1) + (b0+b1)i where (a0+b0i) and (a1+b1i) are the two gaussion integers being added.
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd g0 g1 = (gaussReal g0 + gaussReal g1,gaussImag g0 + gaussImag g1)
{- -----------------------------------------------------------------
 - gaussMult
 - -----------------------------------------------------------------
 - Description: This function returns the product of two gaussion integers by implementing the formula: a0a1-b0b1 + (a0b1+b0a1)i where (a0+b0i) and (a1+b1i) are the two gaussion integers being multiplied.
 -}
gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult g0 g1 = 
    let             
        a0 = gaussReal g0
        a1 = gaussReal g1
        b0 = gaussImag g0
        b1 = gaussImag g1
    in (a0 * a1 - b0 * b1,a0 * b1 + b0 * a1)

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description: This function computes the norm for any gaussion integer by taking the real part of the gaussion integer produced when multiplying a gaussion integer by its conjugate.
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm g = gaussReal gxconjg
    where
        gxconjg = gaussMult g (gaussConj g)

{- -----------------------------------------------------------------
 - maxGaussNorm
 - -----------------------------------------------------------------
 - Description: This function returns the gaussion integer with the highest norm in a given list.
 -}
    
maxGaussNorm :: [GaussianInt] -> GaussianInt
maxGaussNorm gs
    | gs == []       = (0,0)                                          --if an empty list is given, return (0,0)
    | otherwise      = maxGaussNormhelp (tail gs) (head gs)     --otherwise give inputs of the tail of the list and the head of the list to a help function.
   
maxGaussNormhelp :: [GaussianInt] -> GaussianInt -> GaussianInt
maxGaussNormhelp xs provmax                                                                --recursive function defined as the following:
    | xs == []                                 = provmax                                   --if an empty list is given then return the provisional maximum. This is the base case that will end up returning our answer.
    | gaussNorm (head xs) <= gaussNorm provmax  = maxGaussNormhelp (tail xs) provmax       --if the norm of the head of the list is less than or equal to norm of the head of the function then run the function again but wihtout the head of the list.
    | gaussNorm (head xs) > gaussNorm provmax  = maxGaussNormhelp (tail xs) (head xs)      --if the norm of the head of the list is greater than the norm of the provisional maximum then run the function again but with the head of the list being the new provisional maximum.
{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input:(1,2)
 - - Expected Output:(1,-2)
 - - Acutal Output:(1,-2)
  - -----------------------------------------------------------------
 -  -Function: gaussConj
 - - Test Case Number: 2
 - - Input:(1,(-2))
 - - Expected Output:(1,2)
 - - Acutal Output:(1,2)
  - -----------------------------------------------------------------
 -  -Function: gaussConj
 - - Test Case Number: 3
 - - Input:((-5),(-2))
 - - Expected Output:(-5,2)
 - - Acutal Output:(-5,2)
  - -----------------------------------------------------------------
 -  -Function: gaussAdd
 - - Test Case Number: 1
 - - Input:(1,2) (1,2)
 - - Expected Output:(2,4)
 - - Acutal Output:(1,2)
  - -----------------------------------------------------------------
 -  -Function: gaussAdd
 - - Test Case Number: 2
 - - Input:((-2),(-2)) ((-2),(-2))
 - - Expected Output:(-4,-4)
 - - Acutal Output:(-4,-4)
 - -----------------------------------------------------------------
 -  -Function: gaussAdd
 - - Test Case Number: 3
 - - Input:((-2),(-2)) (2,2)
 - - Expected Output:(0,0)
 - - Acutal Output:(0,0)
  - -----------------------------------------------------------------
 -  -Function: gaussMult
 - - Test Case Number: 1
 - - Input:((-15),(1)) ((-15),(-1))
 - - Expected Output:(226,0)
 - - Acutal Output:(226,0)
 - -----------------------------------------------------------------
 -  -Function: gaussMult
 - - Test Case Number: 2
 - - Input:((-1),(-1)) ((-1),1)
 - - Expected Output:(2,0)
 - - Acutal Output:(2,0)
 - -----------------------------------------------------------------
 -  -Function: gaussMult
 - - Test Case Number: 3
 - - Input:(250,459) (34,67)
 - - Expected Output:(-22253,32356)
 - - Acutal Output:(-22253,32356)
 - -----------------------------------------------------------------
 -  -Function: gaussNorm
 - - Test Case Number: 1
 - - Input:(1,1)
 - - Expected Output:2
 - - Acutal Output:2
 - -----------------------------------------------------------------
 -  -Function: gaussNorm
 - - Test Case Number: 2
 - - Input:(2,(-2))
 - - Expected Output:8
 - - Acutal Output: 8
 - -----------------------------------------------------------------
 -  -Function: gaussNorm
 - - Test Case Number: 3
 - - Input:((-1),(-1))
 - - Expected Output:2
 - - Acutal Output:2
 - -----------------------------------------------------------------
 -  -Function: maxGaussNorm
 - - Test Case Number: 1
 - - Input:[]
 - - Expected Output:(0,0)
 - - Acutal Output:(0,0)
 - -----------------------------------------------------------------
 -  -Function: maxGaussNorm
 - - Test Case Number: 2
 - - Input:[(1,2),((-2),(-4))]
 - - Expected Output:(-2,-4)
 - - Acutal Output:(-2,-4)
 - -----------------------------------------------------------------
 -  -Function: maxGaussNorm
 - - Test Case Number: 3
 - - Input:[(1,2),((-2),(-4)),(1,2),(2,4)]
 - - Expected Output:(-2,-4)
 - - Acutal Output:(-2,-4)
 -}

