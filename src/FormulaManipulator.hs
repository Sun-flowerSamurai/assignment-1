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
-- foldE :: (a,b) -> (a,b) -> ((a,b) -> (a,b) -> (a,b)) -> ((a,b) -> (a,b) -> (a,b)) -> Expr a b -> (a,b)
foldE :: a -> a -> (a -> a -> a) -> (a-> a -> a) -> Expr c d -> a --nu moeten ze wel alletwee zelfde type zijn helaas
foldE a b f g = rec
                where
                  rec (Var v) = a --Constructor 1 --kijk ik denk dus niet echt dat dit klopt
                  rec (Const c) = b --Constructor 2
                  rec (Plus exr1 exr2) = f (rec exr1) (rec exr2) --Constructor 3 --Deze kloppen 99% zeker
                  rec (Mult exr1 exr2) = g (rec exr1) (rec exr2) --Constructor 4

foldE' :: (a->a) -> (b->a) -> (a -> a -> a) -> (a-> a -> a) -> Expr a b -> a --neemt nu functies als input
foldE' a b f g = rec
                where
                  rec (Var v) = a v--Constructor 1 --kijk ik denk dus ook niet echt dat dit klopt want je pakt nu een functie
                  rec (Const c) = b c--Constructor 2
                  rec (Plus exr1 exr2) = f (rec exr1) (rec exr2) --Constructor 3 --Deze kloppen 99% zeker
                  rec (Mult exr1 exr2) = g (rec exr1) (rec exr2) --Constructor 4
foldEx :: (x->c) -> (y->c) -> (c -> c -> c) -> (c-> c -> c) -> Expr x y -> c --neemt nu functies als input
foldEx a b f g = rec
                where
                  rec (Var v) = a v--Constructor 1 --kijk ik denk dus ook niet echt dat dit klopt want je pakt nu een functie
                  rec (Const c) = b c--Constructor 2
                  rec (Plus exr1 exr2) = f (rec exr1) (rec exr2) --Constructor 3 --Deze kloppen 99% zeker
                  rec (Mult exr1 exr2) = g (rec exr1) (rec exr2) --Constructor 4
printE    = error "Implement, document, and test this function"

printE' :: Expr String String -> String --werkt alleen met strings nog niet general types
printE' = foldE' id id (\x y -> "(" ++ x ++ " + " ++ y ++ ")") (\x y -> x ++ " * " ++ y ) --overbodige haakjes zou kunnen atm

printEx :: (Show x, Show y) => Expr x y -> String --werkt alleen met strings nog niet general types
printEx = foldEx show show (\x y -> "(" ++ x ++ " + " ++ y ++ ")") (\x y -> x ++ " * " ++ y ) --overbodige haakjes zou kunnen atm


myFoldE :: (a -> c)
  -> (b -> c)
  -> (c -> c -> c)
  -> (c -> c -> c)
  -> Expr a b
  -> c
myFoldE f g h k = rec
                    where
                    rec (Var v) = f v
                    rec (Const c) = g c
                    rec (Plus exr1 exr2) = h (rec exr1) (rec exr2) 
                    rec (Mult exr1 exr2) = k (rec exr1) (rec exr2)



myPrintE :: Expr String Integer -> String
myPrintE = myFoldE id show (\ l r -> "(" ++ l ++ "+" ++ r ++ ")") (\ l r -> l ++ "*" ++ r)


evalE' :: (a -> Integer) -> Expr a Integer -> Integer
evalE' d = myFoldE d id (+) (*) 


evalE     = error "Implement, document, and test this function"



simplifyE = error "Implement, document, and test this function"
diffE     = error "Implement, document, and test this function"
