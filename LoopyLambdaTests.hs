-- I am Insiyah Ujjainwala
-- McMaster email: ujjainwi@mcmaster.ca

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- |
module LoopyLambdaTests (lambdaTests) where

import A3.LoopyLambda

import Test.HUnit

-- | Helper for testing α-equivalence of expressions.
assertAlphaEqual :: String -> Expr -> Expr -> Assertion
assertAlphaEqual msg e1 e2 = assertBool msg (alphaEq e1 e2)

-- These tests are meant to serve as examples for how to use 'HUnit'.
-- !!!! THESE DO NOT COUNT TOWARDS YOUR MARKS, AND SHOULD BE REMOVED BEFORE SUBMISSION !!!!
-- TODO: Delete these tests, and write your own for 'Expr'.

assertAlphaMaybeEqual :: String -> Maybe Expr -> Maybe Expr -> Assertion
assertAlphaMaybeEqual msg (Just e1) (Just e2) = assertBool msg (alphaEq e1 e2)
assertAlphaMaybeEqual msg Nothing Nothing = assertBool msg True
assertAlphaMaybeEqual msg _ _ = assertBool msg False


lambdaTests :: Test
lambdaTests = TestList
  [
  -- Test 1: Edge Case for Zero
  TestCase (assertAlphaMaybeEqual "This case has no output" (stepLoop Zero) Nothing),

  -- Test 2: Edge Case for Var  
  TestCase (assertAlphaMaybeEqual "This case has no output" (stepLoop (Var "5")) Nothing),

  -- Test 3: Edge Case for Lam 
  TestCase (assertAlphaMaybeEqual "This case has no output" (stepLoop (Lam "x" (Var "x"))) Nothing),

  -- Test 4: Edge Case for PlusOne
  TestCase (assertAlphaMaybeEqual "This case has no output" (stepLoop (PlusOne Zero)) Nothing),

   -- Test 5: Edge Case for PlusOne
   TestCase (assertAlphaMaybeEqual "This case has no output" (stepLoop (PlusOne (Var "10"))) Nothing), 

  -- Test 6: Edge Case for PlusOne
  TestCase (assertAlphaMaybeEqual "This case has no output" (stepLoop (PlusOne (Lam "x" (Var "x")))) Nothing),

  -- Test 7: Edge Case for App
  TestCase (assertAlphaMaybeEqual "This case has no output" (stepLoop (App (Var "y") (Var "x"))) Nothing),

  -- Test 8: Edge Case for Loop
  TestCase (assertAlphaMaybeEqual "This case has no output" (stepLoop (Loop (Var "x") (Var "y") (Var "z"))) Nothing),

  -- Test 9: Combining multiple constructors
  TestCase (assertAlphaMaybeEqual "No substitution as there is no instance of x in the lambda expression" 
    (stepLoop (App (Lam "x" (Var "y")) (PlusOne Zero))) 
    (Just (Var "y"))),

  -- Test 10: Edge Case for nested App
  TestCase (assertAlphaMaybeEqual "This case has no output" 
    (stepLoop (App (App (Var "x") (PlusOne(PlusOne Zero))) (PlusOne Zero))) 
    Nothing),

  -- Test 11: Nested App 1
  TestCase (assertAlphaMaybeEqual "Combining lambda and nested app" 
    (stepLoop (App (App (Lam "x" (Var "x")) (PlusOne Zero)) (PlusOne Zero))) 
    (Just (App (PlusOne Zero) (PlusOne Zero)))),  

  -- Test 12: Nested App 2
  TestCase (assertAlphaMaybeEqual "Combining lambda, nested app and PlusOne" 
    (stepLoop (App (App (Lam "x" (Var "x")) (PlusOne(PlusOne (Var "A")))) (PlusOne Zero))) 
    (Just (App (PlusOne (PlusOne (Var "A"))) (PlusOne Zero)))),  

  -- Test 13: Reducing Lambda Expressions
  TestCase (assertAlphaMaybeEqual "x is substituted with hello" 
    (stepLoop (App (Lam "x" (Var "x")) (Var "hello"))) 
    (Just (Var "hello"))),

  -- Test 14: Loop with Zero 
  TestCase (assertAlphaMaybeEqual "Follows the loop reduction rule when expression 1 is Zero" 
    (stepLoop (Loop Zero (Var "x") (Var "y"))) 
    (Just (Var "x"))),

  -- Test 15: Loop with a reducable expression as first argument 
  TestCase (assertAlphaMaybeEqual "Lambda expression reduces to hello, then follows the appropriate loop reduction rule" 
    (stepLoop (Loop (App (Lam "x" (Var "x")) (Var "hello")) (Var "y") (Var "z"))) 
    (Just (Loop (Var "hello") (Var "y") (Var "z")))),
  
  -- Test 16: Loop with PlusOne 
  TestCase (assertAlphaMaybeEqual "Follows the loop reduction rule when expression 1 is PlusOne"
    (stepLoop (Loop (PlusOne (Var "x")) (Var "y") (Var "z"))) (Just (App (Var "z") 
    (Loop (Var "x") (Var "y") (Var "z"))))),

  -- Test 17: Nested loop with Zero 
  TestCase (assertAlphaMaybeEqual "First argument of innermost loop is Zero" 
    (stepLoop (Loop (Loop Zero (Var "x") (Var "y")) (Var "z") (Var "w"))) 
    (Just (Loop (Var "x") (Var "z") (Var "w")))),

  -- Test 18: Nested loop with PlusOne 
  TestCase (assertAlphaMaybeEqual "First argument of innermost loop is PlusOne" 
    (stepLoop (Loop (Loop (PlusOne (Var "A")) (Var "x") (Var "y")) (Var "z") (Var "w"))) 
    (Just (Loop (App (Var "y") (Loop (Var "A") (Var "x") (Var "y"))) (Var "z") (Var "w")))),

  -- Test 19: Edge case for nested loop and App
  TestCase (assertAlphaMaybeEqual "Non reducable arguments for App" 
    (stepLoop(Loop (Loop (App (Var "a") (Var "b")) (Var "c") (Var "d")) (Var "x") (Var "y"))) 
    Nothing),

  -- Test 20: Edge case for loop using lambda
  TestCase (assertAlphaMaybeEqual "Non reducable arguments for App" 
    (stepLoop (Loop (Lam "x" (Var "x")) (Var "y") (Var "z"))) 
    Nothing), 

  -- Test 21: Edge case for loop using app and lambda
  TestCase (assertAlphaMaybeEqual "Non reducable arguments for App" 
    (stepLoop (Loop (App (Lam "x" (Var "x")) (Var "y")) (Var "a") (Var "z"))) 
    (Just (Loop (Var "y") (Var "a") (Var "z")))),

  -- Test 22: Lambda expression with Loop substitution 
  TestCase (assertAlphaMaybeEqual "Var x gets substitution with a loop expression"
  (stepLoop(App (Lam "y" (Var "y")) (Loop (PlusOne Zero) (Var "a") (Var "b"))))
  (Just (Loop (PlusOne Zero) (Var "a") (Var "b")))),

  -- Test cases to compute last 2 digits of student number using the Loopy Lambda language
  -- My student number is 400357483
  -- NOTE: the question was really unclear about the following test cases and the TAs kept making changes to the interpretations of the questions
  -- until a few hours before submission.
  -- I found out that we needed to include a count for how many PlusOne's there are in the output for 'readibility' at 7:00pm on November 10th
  -- and it was after I had made my first submission of the assignment and I have too many other assignments to work on so I couldn't make the 
  -- changes with such short notice. To ensure that my output is correct, I counted the number, so I suggest you count it too to make sure there
  -- are 83.

  -- Test 23
  TestCase (assertAlphaMaybeEqual "Combining 3 PlusOne's with 80 PlusOne's that resulted from Lambda substitution" 
    (stepLoop (PlusOne (PlusOne (PlusOne (App (Lam "x" (Var "x")) (twoDigits 80)))))) 
    (Just (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne Zero)))))))))))))))))))))))))))))))))))))))))))))))))
    )))))))))))))))))))))))))))))))))))),

  -- Test 24
  TestCase (assertAlphaMaybeEqual "Combining 6 PlusOne's with 77 PlusOne's that resulted from Loop Zero reduction" 
    (stepLoop (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (Loop Zero (twoDigits 77) (Var "83"))))))))) 
    (Just (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne Zero)))))))))))))))))))))))))))))))))))))))))))))))))
    )))))))))))))))))))))))))))))))))))),

  -- Test 25
  TestCase (assertAlphaMaybeEqual "Using a mathematical expression to generate a string of 81 successors of Zero and combining with 2 more PlusOnes" 
    (stepLoop (PlusOne (PlusOne (App (Lam "y" (twoDigits (7*11 + 4))) (Var "bye"))))) 
    (Just (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne 
    (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne (PlusOne Zero)))))))))))))))))))))))))))))))))))))))))))))))))
    ))))))))))))))))))))))))))))))))))))

  ]
