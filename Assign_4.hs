{- Assignment 4
 - Name: David Maher Messiha
 - Date: November 15, 2019
 -}
module Assign_4 where

import Test.QuickCheck

macid :: String
macid = "mahermed"



data MathExpr a = X
                | Coef a
                | Sum (MathExpr a) (MathExpr a)
                | Prod (MathExpr a) (MathExpr a)
                | Quot (MathExpr a) (MathExpr a)
                | Exp (MathExpr a)
                | Log (MathExpr a)
                deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - value
 - -----------------------------------------------------------------
 - Description: Compute the value of any math expression that can be formed by the algebraic data type MathExpr
 -}
value :: (Floating a, Eq a) => MathExpr a -> a -> a
value eq n = mathExpr eq
    where
        mathExpr X = n
        mathExpr (Coef x) = x
        mathExpr (Exp x) = exp (mathExpr x)
        mathExpr (Log x) = log (mathExpr x)                                -- Pattern matches all instances of MathExpr to form a value of the corresponding constructor
        mathExpr (Sum x y) = mathExpr x + mathExpr y
        mathExpr (Prod x y) = mathExpr x * mathExpr y
        mathExpr (Quot x y) = mathExpr x / mathExpr y

valueProp :: (Floating a, Eq a, Num a) => a -> Bool
valueProp n = (value X n) == n                         

{- -----------------------------------------------------------------
 - simp
 - -----------------------------------------------------------------
 - Description: Simplifies a MathExpr using simplifying properties of 0 and 1
 -}

simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp u
    | u == simphelp u = u                                   -- Recursive function that ensures the MathExpr is fully simplified
    | otherwise       = simp (simphelp u)
simphelp (Sum (Coef  0.0) u) = simphelp u
simphelp (Sum u (Coef  0.0)) = simphelp u
simphelp (Prod (Coef 1.0) u) = simphelp u
simphelp (Prod u (Coef 1.0)) = simphelp u                   -- All the properties provided by the requirements 
simphelp (Quot u (Coef 1.0)) = simphelp u
simphelp (Prod (Coef 0.0) u) = Coef 0.0
simphelp (Prod u (Coef 0.0)) = Coef 0.0
simphelp (Exp (Coef 0.0))    = Coef 1.0
simphelp (Log (Coef 1.0))    = Coef 0.0
simphelp (X)                 = X
simphelp (Coef x)            = Coef x
simphelp (Exp x)             = Exp x
simphelp (Log x)             = Log x
simphelp (Prod (Coef x) (Coef y)) = Coef (x*y)
simphelp (Sum (Coef x) (Coef y)) = Coef (x+y)
simphelp (Sum u v)
    | (u == simphelp u) && (v == simphelp v) = Sum u v
    | otherwise   = Sum (simphelp u) (simphelp v)                   -- Evaluates the simplification of Sum, Prod, and Quot by seeing if u and v are simplified, then simplifying them if they are not
simphelp (Prod u v)
    | (u == simphelp u) && (v == simphelp v) = Prod u v
    | otherwise   = Prod (simphelp u) (simphelp v)
simphelp (Quot u v)
    | (u == simphelp u) && (v == simphelp v) = Quot u v
    | otherwise   = Quot (simphelp u) (simphelp v) 

simpProp :: (Floating a, Eq a, Num a) => a -> Bool
simpProp n = (value (simp (Exp (Coef 0.0))) n) == 1.0
{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description: Evaluates the dirivative of a MathExpr by using the differentiation rules 
  -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = Coef 1.0
diff (Coef x) = Coef 0.0
diff (Sum x y) = Sum (diff x) (diff y)                                 -- Pattern matching each constructor of MathExpr with the corresponding differentiation rules to differentiate the function
diff (Prod x y) = Sum (Prod (diff x) y) (Prod x (diff y))
diff (Quot x y) = 
    let 
        numerator = (Sum (Prod (diff x) y) (Prod (Prod x (diff y)) (Coef (-1.0))))          -- Broke up the numerator and denominator for the quotient rule to support readability
        denominator = (Prod y y)
        in Quot numerator denominator
diff (Exp x) = Prod (Exp x) (diff x)
diff (Log x) = Prod (Quot (Coef 1.0) (x)) (diff x)

equationoftan :: (Floating a, Eq a) => MathExpr a -> a -> MathExpr a
equationoftan eq a = Sum (Prod (Coef (value (diff eq) a)) (Sum X (Prod (Coef (-1)) (Coef a)))) (Coef (value eq a))   -- Evaluates the eqaution of the tangent by applying the formuala y = f'(a)(x-a) + f(a).
                                                                                                                     -- I can use this for quick check because I can test if the tangent line at point a of the funtion is toching the function at point a, which should always hold true for any function you can differentiate                                                
diffProp :: (Floating a, Eq a, Num a) => a -> Bool
diffProp n = (value (Quot X (Log (Coef 4))) n) == (value (equationoftan (Quot X (Log (Coef 4))) n) n)

{- -----------------------------------------------------------------
 - readDiffWrite
 - -----------------------------------------------------------------
 - Description: Takes a MathExpr contained in a file, simplifies its derivative , then prints that into a new file
-}
readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite f g = do                 
    u <- readFile (f)                            -- Read what is in the function f
    writeFile (g) $ v $ read u                   -- Write into the file g the result of simplifying the derivative of the original MathExpr
v u = (show (simp (diff u))) ++ "\n"


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number:1
 - - Input: value (Prod X X) 3 
 - - Expected Output:9.0
 - - Acutal Output:9.0
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number:2
 - - Input: value (Prod (Sum X X) (Log (Coef 3))) 3
 - - Expected Output: 6.591673732008658
 - - Acutal Output: 6.591673732008658
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number:3
 - - Input: value (Quot (Prod (Sum X X) (Log (Coef 3))) (Exp (Coef 3))) 3 
 - - Expected Output: 0.3281801107541679
 - - Acutal Output: 0.3281801107541679
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 4
 - - Input: value (Coef 3) 4
 - - Expected Output: 3
 - - Acutal Output: 3.0
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 5
 - - Input: Value X  324
 - - Expected Output: 324.0
 - - Acutal Output: 324.0
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 1
 - - Input: simp (Log (Coef 1))
 - - Expected Output: Coef 0.0
 - - Acutal Output: Coef 0.0
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 2
 - - Input: simp (diff (Quot (Prod (Sum X X) (Log (Coef 3))) (Exp (Coef 3))))
 - - Expected Output: Quot (Prod (Prod (Coef 2.0) (Log (Coef 3.0))) (Exp (Coef 3.0))) (Prod (Exp (Coef 3.0)) (Exp (Coef 3.0)))
 - - Acutal Output: Quot (Prod (Prod (Coef 2.0) (Log (Coef 3.0))) (Exp (Coef 3.0))) (Prod (Exp (Coef 3.0)) (Exp (Coef 3.0)))
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 3
 - - Input: simp (Exp (Coef 0.0))
 - - Expected Output: Coef 1.0
 - - Acutal Output: Coef 1.0
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 1
 - - Input: diff (Prod X X)
 - - Expected Output: Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))
 - - Acutal Output: Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 2
 - - Input: diff (Quot (Prod (Coef 3) (Exp (Coef 4))) (Sum (Log (Coef 4)) (Quot X X)))
 - - Expected Output: Quot (Sum (Prod (Sum (Prod (Coef 0.0) (Exp (Coef 4.0))) (Prod (Coef 3.0) (Prod (Exp (Coef 4.0)) (Coef 0.0)))) (Sum (Log (Coef 4.0)) (Quot X X))) (Prod (Prod (Prod (Coef 3.0) (Exp (Coef 4.0))) (Sum (Prod (Quot (Coef 1.0) (Coef 4.0)) (Coef 0.0)) (Quot (Sum (Prod (Coef 1.0) X) (Prod (Prod X (Coef 1.0)) (Coef (-1.0)))) (Prod X X)))) (Coef (-1.0)))) (Prod (Sum (Log (Coef 4.0)) (Quot X X)) (Sum (Log (Coef 4.0)) (Quot X X)))
 - - Acutal Output: Quot (Sum (Prod (Sum (Prod (Coef 0.0) (Exp (Coef 4.0))) (Prod (Coef 3.0) (Prod (Exp (Coef 4.0)) (Coef 0.0)))) (Sum (Log (Coef 4.0)) (Quot X X))) (Prod (Prod (Prod (Coef 3.0) (Exp (Coef 4.0))) (Sum (Prod (Quot (Coef 1.0) (Coef 4.0)) (Coef 0.0)) (Quot (Sum (Prod (Coef 1.0) X) (Prod (Prod X (Coef 1.0)) (Coef (-1.0)))) (Prod X X)))) (Coef (-1.0)))) (Prod (Sum (Log (Coef 4.0)) (Quot X X)) (Sum (Log (Coef 4.0)) (Quot X X)))
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 3
 - - Input: diff (Quot X X)
 - - Expected Output: Quot (Sum (Prod (Coef 1.0) X) (Prod (Prod X (Coef 1.0)) (Coef (-1.0)))) (Prod X X)
 - - Acutal Output: Quot (Sum (Prod (Coef 1.0) X) (Prod (Prod X (Coef 1.0)) (Coef (-1.0)))) (Prod X X)
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 4
 - - Input: diff ((Sum (Log (Coef 4)) (Quot X X)))
 - - Expected Output: Sum (Prod (Quot (Coef 1.0) (Coef 4.0)) (Coef 0.0)) (Quot (Sum (Prod (Coef 1.0) X) (Prod (Prod X (Coef 1.0)) (Coef (-1.0)))) (Prod X X))
 - - Acutal Output: Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))
 - -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 1
 - - Input: "test.txt" "test1return.txt" File Contains = "(Quot (Prod (Coef 3) (Exp (Coef 4))) (Sum (Log (Coef 4)) (Quot X X)))"
 - - Expected Output: "test1return.txt" File Contains = "Quot (Prod (Prod (Prod (Coef 3.0) (Exp (Coef 4.0))) (Quot (Sum X (Prod X (Coef (-1.0)))) (Prod X X))) (Coef (-1.0))) (Prod (Sum (Log (Coef 4.0)) (Quot X X)) (Sum (Log (Coef 4.0)) (Quot X X)))"

 - - Acutal Output: "test1return.txt" File Contains = "Quot (Prod (Prod (Prod (Coef 3.0) (Exp (Coef 4.0))) (Quot (Sum X (Prod X (Coef (-1.0)))) (Prod X X))) (Coef (-1.0))) (Prod (Sum (Log (Coef 4.0)) (Quot X X)) (Sum (Log (Coef 4.0)) (Quot X X)))"

 - -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 2
 - - Input: "test.txt" "test2return.txt" File Contains = "Exp (Coef 0)"
 - - Expected Output: "test2return.txt" File Contains = "Coef 0.0"
 
 - - Acutal Output: "test2return.txt" File Contains = "Coef 0.0"

 - -----------------------------------------------------------------
    - - Function: readDiffWrite
 - - Test Case Number: 3
 - - Input: "test.txt" "test3return.txt" File Contains = "Log (Quot X X)"
 - - Expected Output: "test3return.txt" File Contains = "Prod (Quot (Coef 1.0) (Quot X X)) (Quot (Sum X (Prod X (Coef (-1.0)))) (Prod X X))"
 
 - - Acutal Output: "test3return.txt" File Contains = "Prod (Quot (Coef 1.0) (Quot X X)) (Quot (Sum X (Prod X (Coef (-1.0)))) (Prod X X))"

 - -----------------------------------------------------------------
 -------------------------------------------------------------------
 QuickCheck 
 -------------------------------------------------------------------
 Function: value 
 Property: (value X n) == n 
 Actual Test Result:Pass
  -------------------------------------------------------------------
 Function: simp
 Property: (value (simp (Exp (Coef 0.0))) n) == 1.0
 Actual Test Result:Pass
   -------------------------------------------------------------------
 Function: diff
 Property: (value (Quot X (Log (Coef 4))) n) == (value (equationoftan (Quot X (Log (Coef 4))) n) n)
 Actual Test Result:Pass
 -}

