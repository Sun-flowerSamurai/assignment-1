{-|
Module      : FormulaManipulator
Description : Manipulate formulas and expressions represented by `Expr` values
Copyright   : Mat Verhoeven (1728342)
              David Chen (1742477)

`FormulaManipulator` offers functions to manipulate, evaluate, and print
formulas and expressions represented by `Expr` values.
-}
module FormulaManipulator
  ( foldE
  , printE
  , evalE
  , simplifyE
  , diffE
  )
where

import           ExprLanguage                   ( Expr(Var, Const, Plus, Mult), parseExpr )
import Data.Bifoldable (Bifoldable)
import Control.Arrow (Arrow(first))


foldE :: (a -> c)  -- Var 
  -> (b -> c)      -- Const
  -> (c -> c -> c) -- Plus 
  -> (c -> c -> c) -- Mult
  -> Expr a b      -- Expression to be folded
  -> c             -- Result
-- ^The foldE function acts as a catamorphism factory for the Expr type.
-- The Expr type has a (binary) tree structure made up of four constructors, allowing for such a catamorphism. 
-- foldE takes in four functions, two of which are f and g which act on a and b respectively for given Expr a b. 
-- The other two are h and k, which act on the expressions inside of the Plus and Mult constructors, 
-- somewhat analogous to a function acting on the successor contructor in a catamorphism on the natural numbers.
foldE f g h k = rec
                    where
                    rec (Var v) = f v                              -- Var
                    rec (Const c) = g c                            -- Const
                    rec (Plus exr1 exr2) = h (rec exr1) (rec exr2) -- Plus
                    rec (Mult exr1 exr2) = k (rec exr1) (rec exr2) -- Mult


printE :: (Show b) => Expr String b -- Expression to be pretty-printed
  -> String                       -- Pretty-printed expression
-- ^Pretty-prints an expression, preserving the right order of operations
-- by placing parentheses (very generously) around the plus operator.
-- parseExpr is a left inverse of this function.
printE = foldE id show (\ l r -> "(" ++ l ++ "+" ++ r ++ ")") (\ l r -> l ++ "*" ++ r)
  

evalE :: (a -> Integer) -- dictionary containing the values
  -> Expr a Integer     -- expression to be evaluated
  -> Integer            -- final result
-- ^Evaluates an expression where (some) variables are replaced 
-- by other expressions (which in this case have to be constants)
evalE d = foldE d id (+) (*)


simplifyE :: (Num b, Eq b) => Expr a b -- Expression to be simplified
  -> Expr a b                          -- Simplified expression
-- ^Simplifies expressions using the following rules:
--    For addition:
--      Constants are added together
--      Adding zero to an expression is the same as that expression
--    For multiplication:
--      Constants are multiplied together
--      Multiplying an expression by 0 returns Const 0
--      Multiplying an expression by 1 simply returns the expression
simplifyE = foldE Var Const addE multE 
  where 
    addE (Const c1) (Const c2)  = Const (c1 + c2) 
    addE (Const c) e            = if c == 0 then e else Plus (Const c) e
    addE e (Const c)            = if c == 0 then e else Plus e (Const c)
    addE e1 e2                  = Plus e1 e2
    
    multE (Const c1) (Const c2) = Const (c1 * c2) 
    multE (Const c) e
      | c == 0                  = Const 0 
      | c == 1                  = e
      | otherwise               = Mult (Const c) e
    multE e (Const c)
      | c == 0                  = Const 0 
      | c == 1                  = e
      | otherwise               = Mult e (Const c) 
    multE e1 e2                 = Mult e1 e2


diffE :: (Eq a, Num b) => a -- The variable with respect to which to differentiate.
  -> Expr a b               -- The expression that needs to be differentiated
  -> Expr a b               -- The resulting derivative
-- ^Differentiates an expression with respect to some variable.
-- The differentiation rules that are implemented are:
--     The constant rule: differentiating a constant results in 0
--     The sum rule: (f(x) + g(x))' = f'(x) + g'(x)
--     The product rule: (f(x)*g(x))' = f'(x)*g(x) + f(x)*g'(x)
diffE x = snd . foldE myVar myConst myAdd myMult
      where
        myVar v                    = (Var v, if v == x then Const 1 else Const 0)
        myConst c                  = (Const c, Const 0)
        myAdd (le, le') (re, re')  = (Plus le re, Plus le' re')
        myMult (le, le') (re, re') = (Mult le re, Plus (Mult le' re) (Mult le re'))
