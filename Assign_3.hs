{- Assignment 3
 - Name: David Maher Messiha
 - Date:  November 2, 2019
 -}
module Assign_3 where

import qualified Data.Map.Strict as IM

macid :: String
macid = "mahermed"

data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show


newtype PolyList a = PolyList [a]
  deriving Show

{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: Evaluates a polynomial at n when given a polynomial of type Poly and n of type a with a type constraint Num
 -}

polyValue :: Num a => Poly a -> a -> a
polyValue p n = poly p                                -- Creating a definition for the the constructors of Poly a to evaluate the polynomial at n
  where
    poly (X)        = n
    poly (Coef x)   = x
    poly (Sum x y)  = poly x + poly y
    poly (Prod x y) = poly x * poly y

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: Evaluates a polynomial list at n when given a polynomial list and n with type constraints Num and Eq
 -}

polyListValue :: (Num a,Eq a) => PolyList a -> a -> a
polyListValue (PolyList p1) n
  | p1 == []  = 0
  | otherwise = (head p1) + n * (polyListValue (PolyList (tail p1)) n)      --each time the function is recursed each value is multiplied by one more n in order to follow the form of a poly list

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: Returns the sum of two polynomials in the form of a polynomial list with type constraint Num and Eq
 -}

polyListSum :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList p1) (PolyList q1)
  | (p1 == []) && (q1 == []) = PolyList []                                  -- the sum of two zero polynomials is a zero polynomial
  | p1 == [0]                = PolyList q1                                  -- if one polynomail is an empty list, the sum is equal to the other polynomial
  | q1 == [0]                = PolyList p1
  | p1 == []                 = PolyList q1                                  -- if one polynomail is an empty list, the sum is equal to the other polynomial
  | q1 == []                 = PolyList p1
  | otherwise                = PolyList (polyListSumHelp (PolyList p1) (PolyList q1))     -- call another function to make code more readable
  where
    polyListSumHelp (PolyList []) (PolyList [])         = []
    polyListSumHelp (PolyList []) (PolyList q1)         = q1
    polyListSumHelp (PolyList p1) (PolyList [])         = p1
    polyListSumHelp (PolyList [x]) (PolyList [y])       = [x + y]                       -- adds all like terms and returns it in the form of a list
    polyListSumHelp (PolyList (x:xs)) (PolyList (y:ys)) = [x + y] ++ (polyListSumHelp (PolyList xs) (PolyList ys))

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: Computes the degree of a polynomial list
 -}

polyListDegree :: (Num a,Eq a) => PolyList a -> Integer
polyListDegree (PolyList p1)
  | p1 == []               = error "Undefined: Zero Polynomial"           -- the degree of a zero polynomail and a polynomial list where it's last term is 0 is undefined
  | head (reverse p1) == 0 = error "Undefined: am = 0 in PolyList"
  | otherwise              = toInteger (length p1) -1                     -- since the length function returns an int I used a function "toInteger" to convert an int to an integer

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: Computes the products of two polynomial lists with type constraint Num and Eq
 -}

polyListProd :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList p1) (PolyList q1)
  | (p1 == []) && (q1 == [])                                                   = PolyList []    -- if one or both of the inputs are zero polynomials the function returns a zero polynomial
  | p1 == []                                                                   = PolyList []
  | q1 == []                                                                   = PolyList []
  | length p1 > length q1                                                      = polyListProd (PolyList q1) (PolyList p1)   -- the way the function is constructed the largest polynomial list must be the first input and since multiplication is commutative the function holds
  | (polyListDegree (PolyList p1) == 0) && (polyListDegree (PolyList q1) == 0) = PolyList [(head p1) * (head q1)]
  | length p1 == 2                                                             = PolyList (([(head p1) * (head q1)]) ++ (addListCoefs (tail (multThroughList (head p1) q1)) (init (multThroughList (head (reverse p1)) q1))) ++ [(head (reverse p1)) * (head (reverse q1))])                                                                                                        -- computing the multiplication of the two polynomials by using cases and a help function to make code more readable
  | polyListDegree (PolyList p1) == 0                                          = PolyList (multThroughList (head p1) q1)
  | otherwise                                                                  = PolyList (([(head p1) * (head q1)]) ++ (addListCoefs (tail (multThroughList (head p1) q1)) (addListCoefs (polyListProdHelp (tail p1) q1) ((numberOf0s (polyListDegree (PolyList p1) -1))++(init (multThroughList (head (reverse p1)) q1))))) ++ [(head (reverse p1)) * (head (reverse q1))])
 
polyListProdHelp :: (Num a,Eq a) => [a] -> [a] -> [a]
polyListProdHelp [x] q1 = []                                                                               --essentially splits up the head, middle and last element of the product then adds them all
polyListProdHelp (x:xs) (ys) = addListCoefs (multThroughList x ys) ([0]++(polyListProdHelp xs ys))
 
numberOf0s :: Num a => Integer -> [a]
numberOf0s 0 = []                                   -- in order to achieve the right power when multiplying, my method consisted of adding zeros to lists
numberOf0s n = [0] ++ numberOf0s (n-1)
  
multThroughList :: Num a => a -> [a] -> [a]
multThroughList x [] = []                                               --easily accessed function to multiply through a list 
multThroughList x (y:ys) = [x * y] ++ (multThroughList x ys)

addListCoefs :: Num a => [a] -> [a] -> [a]
addListCoefs [] [] = []                                                     -- easier way to add coef of lists
addListCoefs [] xs = xs
addListCoefs xs [] = xs
addListCoefs [x] [y] = [x + y]
addListCoefs (x:xs) (y:ys) = [x+y] ++ addListCoefs xs ys

{-
--------------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: Converts a polynomial list to a polynomial in standard form with type constraint Num
 -}

polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList p1) = pListToPoly p1
  where
    pListToPoly []     = Coef 0                 -- the zero polynomial is represented as coefficient 0 by itself
    pListToPoly [x]    = Coef x                 -- cases are constructed for all forms of a polynomial list and the function uses a help function to make code more readable
    pListToPoly [x,y]  = Sum (Coef x) (Prod (Coef y) X)
    pListToPoly (x:xs) = Sum (pListToPoly [x,(head xs)]) (polyListToPolyHelp (tail xs))

polyListToPolyHelp :: Num a => [a] -> Poly a
polyListToPolyHelp []     = Coef 0
polyListToPolyHelp (x:xs) = Sum (Prod (Prod (Coef x) X) X) (Prod (polyListToPolyHelp xs) X)

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: Converts a polynomial in standard form to a polynomial list with type constraints Num and Eq
 -}

polyToPolyList :: (Num a,Eq a) => Poly a -> PolyList a
polyToPolyList p
  | (polyValue p 1) == 0 = PolyList []
  | otherwise = (pToPolyList p)
  where
    pToPolyList (X)                              = PolyList [0,1]
    pToPolyList (Prod X X)                       = PolyList [0,0,1]
    pToPolyList (Sum X X)                        = PolyList [0,2]
    pToPolyList (Coef x)                         = PolyList [x]                                     -- all the cases that match the polynomial list in standard form to a polynomial
    pToPolyList (Prod (Coef x) X)                = PolyList [0,x]
    pToPolyList (Prod x y)                       = polyListProd (pToPolyList x) (pToPolyList y)
    pToPolyList (Sum x y)                        = polyListSum (pToPolyList x) (pToPolyList y)
    
{-
    - -----------------------------------------------------------------
    - Test Cases
    - -----------------------------------------------------------------
    -
    - -----------------------------------------------------------------
    - - Function: polyValue
    - - Test Case Number: 1
    - - Input: (Prod X X) 2
    - - Expected Output: 4
    - - Acutal Output:4
     - -----------------------------------------------------------------
    -  -Function: polyValue
    - - Test Case Number: 2
    - - Input: (Sum (Prod (Coef 3) X) (Coef 3)) 1
    - - Expected Output: 6
    - - Acutal Output: 6
     - -----------------------------------------------------------------
    -  -Function: polyValue
    - - Test Case Number: 3
    - - Input: (Coef 4) 100
    - - Expected Output: 4
    - - Acutal Output: 4
     - -----------------------------------------------------------------
    -  -Function: polyListValue
    - - Test Case Number: 1
    - - Input: (PolyList [1,2,3,4]) 2
    - - Expected Output: 49
    - - Acutal Output: 49
     - -----------------------------------------------------------------
    -  -Function: polyListValue
    - - Test Case Number: 2
    - - Input: (PolyList [0,0,0,1]) 3
    - - Expected Output:27
    - - Acutal Output: 27
    - -----------------------------------------------------------------
    -  -Function: polyListValue
    - - Test Case Number: 3
    - - Input: (PolyList []) 2
    - - Expected Output: 0
    - - Acutal Output: 0
     - -----------------------------------------------------------------
    -  -Function: polyListSum
    - - Test Case Number: 1
    - - Input:(PolyList [1,2,3,4]) (PolyList [1,2,3,4])
    - - Expected Output: PolyList [2,3,6,8]
    - - Acutal Output: PolyList [2,3,6,8]
    - -----------------------------------------------------------------
    -  -Function: polyListSum
    - - Test Case Number: 2
    - - Input:(PolyList [0]) (PolyList [1,2,3])
    - - Expected Output: PolyList [1,2,3]
    - - Acutal Output: PolyList [1,2,3]
    - -----------------------------------------------------------------
    -  -Function: polyListSum
    - - Test Case Number: 3
    - - Input:(PolyList [0,1,2,3,4]) (PolyList [1,2,3])
    - - Expected Output: PolyList [1,3,5,3,4]
    - - Acutal Output:PolyList [1,3,5,3,4]
    - -----------------------------------------------------------------
    -  -Function: polyListProd
    - - Test Case Number: 1
    - - Input:(PolyList [0,0,0,4]) (PolyList [0,0,0,4])
    - - Expected Output:PolyList [0,0,0,0,0,0,16]
    - - Acutal Output:PolyList [0,0,0,0,0,0,16]
    - -----------------------------------------------------------------
    -  -Function: polyListProd
    - - Test Case Number: 2
    - - Input:(PolyList [1,2,3,4]) (PolyList [1,2,3])
    - - Expected Output:PolyList [1,4,10,16,17,12]
    - - Acutal Output: PolyList [1,4,10,16,17,12]
    - -----------------------------------------------------------------
    -  -Function: polyListProd
    - - Test Case Number: 3
    - - Input:(PolyList [1,2,3]) (PolyList [1,2,3,4])
    - - Expected Output: PolyList [1,4,10,16,17,12]
    - - Acutal Output: PolyList [1,4,10,16,17,12]
    - -----------------------------------------------------------------
    -  -Function: polyListToPoly
    - - Test Case Number: 1
    - - Input: (PolyList [1,2,3,4])
    - - Expected Output: Sum (Sum (Coef 1) (Prod (Coef 2) X)) (Sum (Prod (Prod (Coef 3) X) X) (Prod (Sum (Prod (Prod (Coef 4) X) X) (Prod (Coef 0) X)) X))
    - - Acutal Output: Sum (Sum (Coef 1) (Prod (Coef 2) X)) (Sum (Prod (Prod (Coef 3) X) X) (Prod (Sum (Prod (Prod (Coef 4) X) X) (Prod (Coef 0) X)) X))
    - -----------------------------------------------------------------
    -  -Function: polyListToPoly
    - - Test Case Number: 2
    - - Input: (PolyList [0,0,0,1])
    - - Expected Output:Sum (Sum (Coef 0) (Prod (Coef 0) X)) (Sum (Prod (Prod (Coef 0) X) X) (Prod (Sum (Prod (Prod (Coef 1) X) X) (Prod (Coef 0) X)) X))
    - - Acutal Output:Sum (Sum (Coef 0) (Prod (Coef 0) X)) (Sum (Prod (Prod (Coef 0) X) X) (Prod (Sum (Prod (Prod (Coef 1) X) X) (Prod (Coef 0) X)) X))
    - -----------------------------------------------------------------
    -  -Function: polyListToPoly
    - - Test Case Number: 3
    - - Input: (PolyList [])
    - - Expected Output: Coef 0
    - - Acutal Output: Coef 0
    - -----------------------------------------------------------------
    -  -Function: polyToPolyList
    - - Test Case Number: 1
    - - Input: (Prod (Prod X X) X)
    - - Expected Output: PolyList [0,0,0,1]
    - - Acutal Output: PolyList [0,0,0,1]
    - -----------------------------------------------------------------
    -  -Function: polyToPolyList
    - - Test Case Number: 2
    - - Input: (Sum (Prod X X) (Prod (Coef 3) X))
    - - Expected Output: PolyList [0,3,1]
    - - Acutal Output:(-2,-4)
    - -----------------------------------------------------------------
    -  -Function: polyToPolyList
    - - Test Case Number: 3
    - - Input: (Sum (Sum (Prod X X) (Prod (Coef 3) X)) (Coef 3)) 
    - - Expected Output: PolyList [3,3,1]
    - - Acutal Output: PolyList [3,3,1]
     - -----------------------------------------------------------------
    -  -Function: polyToPolyList
    - - Test Case Number: 4
    - - Input: Coef 0
    - - Expected Output: PolyList []
    - - Acutal Output: PolyList []
-}
   
   