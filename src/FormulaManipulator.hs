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


foldE :: (a -> c)  -- Var 
  -> (b -> c)      -- Const
  -> (c -> c -> c) -- Plus 
  -> (c -> c -> c) -- Mult
  -> Expr a b      -- Expression to be folded
  -> c             -- Result
-- ^Catamorphism factory for the Expr type.
-- the Expr type has a (binary) tree structure
foldE f g h k = rec
                    where
                    rec (Var v) = f v                              -- Var
                    rec (Const c) = g c                            -- Const
                    rec (Plus exr1 exr2) = h (rec exr1) (rec exr2) -- Plus
                    rec (Mult exr1 exr2) = k (rec exr1) (rec exr2) -- Mult


printE :: Show b => Expr String b -- Expression to be pretty-printed
  -> String                       -- Pretty-printed expression
-- ^Pretty-prints an expression, preserving the right order of operations
-- by placing parentheses (very generously)
printE = foldE id show (\ l r -> "(" ++ l ++ "+" ++ r ++ ")") (\ l r -> l ++ "*" ++ r)
  
  
printE' :: Show b => Expr String b -- Expression to be pretty-printed
  -> String                        -- Pretty-printed expression
-- ^Another take on pretty-printing, the solution is unfortunately very inefficient :(
-- and it is still incorrect I just realised
printE' = foldE id show (\ l r -> l ++ "+" ++ r) printMult
  where 
    printMult l r
      | '+' `elem` l && '+' `elem` r = "(" ++ l ++ ")*(" ++ r ++ ")"
      | '+' `elem` l                 = "(" ++ l ++ ")*" ++ r
      | '+' `elem` r                 = l ++ "*(" ++ r ++")"
      | otherwise                    = l ++ "*" ++ r


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
--      constants are added together
--      adding zero to an expression is the same as that expression
--    For multiplication:
--      Multiplying an expression by 0 return Const 0
--      Multiplying an expression by 1 simply return the expression
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
      | otherwise               = Mult (Const c) e
    multE e1 e2                 = Mult e1 e2


-- Still need to implement diffE :(
diffE    = undefined
