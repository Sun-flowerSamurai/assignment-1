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
-- ^A function which processes the arguments given in the Command Line Interface.
-- Works with printing (-p), simplifying (-s), evaluating (-e) and differentiating (-d) expressions.
-- A help message with more details can be called by inputting "-h" or "--help" into the CLI.
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
        simplify [] = "No expression to simplify."                        --No expression
        simplify arg = (printE . simplifyE) (parse (head arg))
        eval [] = "No expression to evaluate, no variable-to-value dictionary." --No expression, variable map
        eval (x:[]) = "No expression to evaluate."                        --No expression
        eval (x:xz)  = show (evalE (dict x) (parse (last xz)))
          where
            dict xs z =  rec (map (endBy "=") (endBy ";" xs))            --Enforces the mapping format
              where 
                rec []       = error "Not all variables in lookup table."
                rec (xs:xss) = if z == head xs then read (last xs) else rec xss --This gives an error msg right now, Prelude.read: no parse if dict is bad, don't know how to fix
        diff [] = "No expression to derivate, no variable to take the derivative to." --No expression, variable given
        diff (x:[]) = "No expression to derivate."                        --No expression
        diff (x:xs)  = printE (simplifyE (diffE x (parse (last xs))))
        parse xs     = fromRight (Var "Error, couldn't parse expression.") (parseExpr xs) --If parseExpr fails, it shows the error, otherwise it continues with the result

help :: [Char]
help = "This is our Command Line Formula manipulator! \n\
      \  This program works with simple expressions containing \n\
      \  variables and constants, and support addition (+) and \n\
      \  multiplication (*). \n\
      \  To call a particular function use the following format: \n\
      \  specify the function, provide its arguments \n\
      \  and lastly provide the expression you want to operate on. \n\
      \  Example: \"-p \"(x+2)\" \" \n\
      \ \n\
      \  This program has several commands:\n\
      \  -- It can print expressions using -p or --print \n\
      \  -- It can simplify expressions using -s or --simplify \n\
      \  -- It can evaluate expressions using -e or --evaluate \n\
      \  -- It can differentiate expressions using -d or --differentiate\n\
      \  -- It can show this help message with -h or --help \n\
      \ \n\
      \  To evaluate an expression you must provide an additional \n\
      \  argument. Namely the replacement values. These must be formatted \n\
      \  as <var>=<value>, evaluate expression with multiple variables \n\
      \  multiple replacement values need to be provided, these need \n\
      \  to be separated by the ';' character. \n\
      \  Example: \"-e \"x=2;\" \"(x+2)\" \" returns 4.\n\
      \ \n\
      \  The program can also differentiate expressions in a given variable, \n\
      \  this variable must be provided as an argument. \n\
      \  Example: \"-d \"x\" \"(x+2)\"\" returns 1."

errormsg :: String
errormsg = "Command not recognized, type \"-h\" for a help message."