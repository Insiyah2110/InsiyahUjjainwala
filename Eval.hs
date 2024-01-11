-- ujjainwi@mcmaster.ca
-- I am Insiyah Ujjainwala

-- Evaluator for Assignment 1.
module A1.Eval where

data Expr =
    EInt Integer
  | ECh Char
  | EBool Bool
  | EString String
  | EAdd Expr Expr    -- addition of integers
  | EMul Expr Expr    -- multiplication of integers
  | ENeg Expr         -- negation of integers
  | ECat Expr Expr    -- concatenation of strings
  | ECons Char Expr   -- adding a character at the start of a string
  | EAnd Expr Expr    -- AND of two booleans
  | EXor Expr Expr    -- XOR of two booleans
  | EIf Expr Expr Expr -- if-then-else
  | EShowInt Expr      -- render an integer as a string
  deriving (Show)

data Val =
    VInt Integer
  | VBool Bool
  | VString String
  | VError             -- something went wrong
  deriving (Show, Eq)

evalExpr :: Expr -> Val

evalExpr (EInt i) = VInt i
evalExpr (ECh ch) = VString [ch]
evalExpr (EBool tf) = VBool tf
evalExpr (EString str) = VString str

-- Runs evalExpr on both inputs, if they are converted to type VInt, then calculates the sum.
-- Otherwise returns VError.
evalExpr (EAdd num1 num2)= case (evalExpr num1, evalExpr num2) of
  (VInt i, VInt j) -> VInt (i + j)
  (_)              -> VError

-- Runs evalExpr on both inputs, if they are converted to type VInt, then calculates the product.
-- Otherwise returns VError.
evalExpr (EMul num1 num2) = case (evalExpr num1, evalExpr num2) of
  (VInt i, VInt j) -> VInt (i * j)
  (_)              -> VError

-- Runs evalExpr on the input, if it is converted to type VInt, then negates the integer.
-- Otherwise returns VError.
evalExpr (ENeg num) = case (evalExpr num) of
  VInt i -> VInt (-i)
  (_)    -> VError

-- Runs evalExpr on both inputs, if they are converted to type VString, then concatenates the first expression to the second.
-- Otherwise returns VError.
evalExpr (ECat exp1 exp2) = case (evalExpr exp1, evalExpr exp2) of
  (VString x, VString y) -> VString (x ++ y)
  (_)                    -> VError

-- Takes a character and expression as arguments.
-- Runs evalExpr on the second arguement, if it is converted to type VString, then concatenates the character to the string.
-- If the first argument is not a character and/or the second is not a string, it returns VError.
evalExpr (ECons c exp) = case (evalExpr exp) of
  VString s -> VString (c : s)
  (_)       -> VError

-- Takes 2 boolean values as arguments. 
-- Runs evalExpr on each boolean value, if they get converted to VBool then performs the logical AND operator on them.
    -- NOTE: logical AND returns True only when both inputs are True, and False otherwise
-- Otherwise returns VError.
evalExpr (EAnd b1 b2) = case (evalExpr b1, evalExpr b2) of
  (VBool i, VBool j) -> VBool (i && j)
  (_)                -> VError

-- Takes 2 boolean values as arguments. 
-- Runs evalExpr on each boolean value, if they get converted to VBool then performs the logical XOR operator on them.
    -- NOTE: XOR returns True when either inputs is True (but not both), and False otherwise.
-- Otherwise returns VError.
evalExpr (EXor b1 b2) = case (evalExpr b1, evalExpr b2) of
  (VBool i, VBool j) -> VBool (i /= j)
  (_) -> VError

-- Takes an if condition, and 2 expressions as arguments.
-- Runs evalExpr to see the if condition is of type EBool.
-- After conversion to VBool, if the condition is true, EIf evaluates the first expression.
-- Otherwise, it evaluates the second
-- If the if condition is NOT of type boolean - it returns VError
evalExpr (EIf ifCond thenExpr elseExpr) = case (evalExpr ifCond) of
  VBool True -> evalExpr thenExpr
  VBool False -> evalExpr elseExpr
  (_) -> VError

-- Takes an Int argument and runs evalExpr to convert to VInt. Then changes its type to VString
-- If argument is NOT of type Int - it returns VError.
evalExpr (EShowInt e) = case evalExpr e of
  VInt x -> VString (show x)
  (_) -> VError






