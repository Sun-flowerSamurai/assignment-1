{-|
Module      : FormulatorCLI
Description : The command-line interface for the formulator program
Copyright   : STUDENT NAME 1 (ID)
              STUDENT NAME 2 (ID)

The command-line interface for the formulator program. Parses and processes
command-line arguments. Manipulates expressions according to the specification
in terms of command-line arguments.
-}
module FormulatorCLI
  ( processCLIArgs
  )
where
import           Data.Either
import           Data.List.Split
import           ExprLanguage                   ( Expr (..)
                                                , parseExpr
                                                , ParseError
                                                )
import           FormulaManipulator             ( printE
                                                , evalE
                                                , simplifyE
                                                , diffE
                                                )

processCLIArgs :: [String] -> String
processCLIArgs as = func (head as) (tail as)
  where
    func "-p" xs =  printE (fromRight (Var "error") (parseExpr (head xs)))
