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

-- foldE :: Expr a b -> Expr a b -> (Expr a b -> Expr a b -> Expr a b) -> (Expr a b -> Expr a b -> Expr a b) -> Expr a b -> Expr a b -- het idee is om foldE op elke constructor te laten werken! dus foldE.a.b.f.g werkt op een Expr type. Hier hoort a bij const, b bij var, f bij plus en g bij mult (bijvoorbeeld)
-- foldE     = error "Implement, document, and test this function"
-- Willen we tuples? Ik weet niet helemaal of dit handig is
foldE :: (a,b) -> (a,b) -> ((a, b) -> (a,b) -> (a,b)) -> ((a,b) -> (a,b) -> (a,b)) -> Expr c d -> (a,b)
foldE a b f g = rec
                where
                  rec (Var v) = a --Constructor 1
                  rec (Const c) = b --Constructor 2
                  rec (Plus exr1 exr2) = f (rec exr1) (rec exr2) --Constructor 3 
                  rec (Mult exr1 exr2) = g (rec exr1) (rec exr2) --Constructor 4
printE    = error "Implement, document, and test this function"
evalE     = error "Implement, document, and test this function"
simplifyE = error "Implement, document, and test this function"
diffE     = error "Implement, document, and test this function"
