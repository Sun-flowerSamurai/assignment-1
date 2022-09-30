{-|
Module      : FormulatorCLI
Description : The command-line interface for the formulator program
Copyright   : Matt Verhoeven (1728342)
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


processCLIArgs :: [String] -> String -- Input as a list of strings to output, which is a single string
processCLIArgs [] = "Please input command or type \"-h\" for help" -- nothing inputted
processCLIArgs (arg:args)
    | arg == "-p" || arg == "--print"         = printer args --prints expression
    | arg == "-s" || arg == "--simplify"      = simplify args --simplifies and prints expression
    | arg == "-e" || arg == "--evaluate"      = eval args --evaluates expression with every variable being mapped to a value
    | arg == "-d" || arg == "--differentiate" = diff args -- differentiates, simplifies and then prints expression
    | arg == "-h" || arg == "--help"          = help --shows help message
    | otherwise = errormsg --no valid command, shows how to access help message
      where
        printer [] = "No expression to print."                           --No expression
        printer arg    = printE (parse (head arg))
        simplify [] = "No expression to simplify"                        --No expression
        simplify arg = (printE . simplifyE) (parse (head arg))
        eval [] = "No expression to evaluate, no variable-to-value dictionary" --No expression, variable map
        eval (x:[]) = "No expression to evaluate"                        --No expression
        eval (x:xs)  = show (evalE (dict x) (parse (last xs)))
          where
            dict xs z =  rec (map (endBy "=") (endBy ";" xs))            --Enforces the mapping format
              where 
                rec []       = error "Variable not in lookup table."
                rec (xs:xss) = if z == head xs then read (last xs) else rec xss --This gives an error msg right now, Prelude.read: no parse if dict is bad
        diff [] = "No expression to derivate, no variable to take the derivative to" --No expression, variable given
        diff (x:[]) = "No expression to derivate"                        --No expression
        diff (x:xs)  = printE (simplifyE (diffE x (parse (last xs))))
        parse xs     = fromRight (Var "Error, couldn't parse expression.") (parseExpr xs) --If parseExpr fails, it shows the error, otherwise it continues with the result

help :: [Char]
help = "This is our Command Line Formula manipulator! \n\
      \  This programs works with simple expressions containing: \n\
      \  variables and constants and support addition (+) and \n\
      \  multiplication (*). \n\
      \  to call a particular function use the following format \n\
      \  specify the function, then provide its arguments \n\
      \  and lastly provide the expression you want to operate on \n\
      \  This program can print expressions using -p or --print \n\
      \  It can simplify expressions using -s or --simplify \n\
      \  It can evaluate expression using -e or --evaluate \n\
      \  To evaluate an expression you must provide an additional \n\
      \  argument. Namely the replacement values. These must be formatted \n\
      \  as <var>=<value>, evaluate expression with multiple variables \n\
      \  multiple replacement values need to be provided, these need \n\
      \  to be separated by the ';' character. \n\
      \  The program can also differentiate expressions in a given variable \n\
      \  this variable must be provided as an argument"

errormsg :: String
errormsg = "Command not recognized, type \"-h\" for a help message."