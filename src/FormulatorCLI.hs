{-|
Module      : FormulatorCLI
Description : The command-line interface for the formulator program
Copyright   : Mat Verhoeven (1728342)
              David Chen (1742477)

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
processCLIArgs (arg:args)
    | arg == "-p" || arg == "--print"         = print args
    | arg == "-s" || arg == "--simplify"       = simplify args
    | arg == "-e" || arg == "--evaluate"      = eval args
    | arg == "-d" || arg == "--differentiate" = diff args
    | arg == "-h" || arg == "--help"          = help
      where
        print arg    = printE (parse (head arg))
        simplify arg = (printE . simplifyE) (parse (head arg))
        eval (x:xs)  = show (evalE (dict x) (parse (last xs)))
          where
            dict xs z =  rec (map (endBy "=") (endBy ";" xs))
              where 
                rec []       = error "Variable not in lookup table"
                rec (xs:xss) = if z == head xs then read (last xs) else rec xss
        diff (x:xs)  = printE (simplifyE (diffE x (parse (last xs))))
        parse xs     = fromRight (Var "error") (parseExpr xs)

help :: [Char]
help = "This is our Command Line Formula manipulator! \n\
      \ This programs works with simple expressions containing: \n\
      \ variables and constants and support addition (+) and \n\
      \ multiplication (*). \n\
      \ This program can print expressions using -p or --print \n\
      \ It can simplify expressions using -s or --simplify \n\
      \ It can evaluate expression using -e or --evaluate \n\
      \ To evaluate an expression you must provide an additional \n\
      \ argument. Namely the replacement values. These must be formatted \n\
      \ as <var>=<value>, evaluate expression with multiple variables \n\
      \ multiple replacement values need to be provided, these need \n\
      \ to be separated by the ';' character. \n\
      \ The program can also differentiate expressions in a given variable \n\
      \ this variable must be provided as an argument"