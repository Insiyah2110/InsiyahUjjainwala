-- I am Insiyah Ujjainwala
-- McMaster email: ujjainwi@mcmaster.ca

{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | Tests for question 1
module SKITests (skiTests) where

import A3.SKI
import Test.HUnit

-- TODO: Write tests for 'SKI'.
skiTests :: Test
skiTests = TestList [

    -- Test 1: Edge case for S
    TestCase (assertEqual "Single characters have no reduction" Nothing (ski S)),

    -- Test 2: Edge case for K
    TestCase (assertEqual "Single characters have no reduction" Nothing (ski K)),


    -- Test 3: Edge case for I
    TestCase (assertEqual "Single characters have no reduction" Nothing (ski I)),

    -- Test 4: Reduction for I
    TestCase (assertEqual "I applied to any character x results in x" (Just S) (ski(App I S))),

    -- Test 5: Reduction for I
    TestCase (assertEqual "I applied to any character x results in x" (Just K) (ski(App I K))),
  
    -- Test 6: Reduction for I
    TestCase (assertEqual "I applied to any character x results in x" (Just I) (ski(App I I))),

    -- Test 7: Reduction for K
    TestCase (assertEqual "K applied to any 2 characters x and y, results in x" (Just S) (ski (App (App K S) I))),

    -- Test 8: Reduction for K
    TestCase (assertEqual "K applied to any 2 characters x and y, results in x" (Just K) (ski (App (App K K) S))),

    -- Test 9: Reduction for K
    TestCase (assertEqual "K applied to any 2 characters x and y, results in x" (Just I) (ski (App (App K I) K))),

    -- Test 10: Reduction for S
    TestCase (assertEqual "S applied to any 3 characters x, y and z, results in xz(yz)" (Just (App (App I K) (App K K))) (ski (App (App (App S I) K) K))),

    -- Test 11: Reduction for S
    TestCase (assertEqual "S applied to any 3 characters x, y and z, results in xz(yz)" (Just (App (App I S) (App K S))) (ski (App (App (App S I) K) S))),

    -- Some test cases for App e1 e2
    -- App e1 e2 only performs a reduction if e1 and/or e2 are of form Ix or Kxy or Sxyz - if not, results 'Nothing'

    -- Test 12: Edge case
    TestCase (assertEqual "No reduction rules can be applied" Nothing (ski (App S K))),

    -- Test 13: Edge case
    TestCase (assertEqual "No reduction rules can be applied" Nothing (ski (App K (App K I)))),

    -- Test 14: Combining reduction rules
    TestCase (assertEqual "Applying reduction rule S to an expression undergoing reduction by I" (Just (App S I)) (ski (App S (App I I)))),

    -- Test case 15: Combining reduction rules
    TestCase (assertEqual "Applying reduction on 2 arguments, each of which are applying a reduction rule" (Just S) (ski (App (App K S) (App I I))))

    ]
