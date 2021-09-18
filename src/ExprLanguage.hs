{-|
   Module       : ExprLanguage
   Description  : Expression language data types and accompanying parser
   Copyright    : (c) Huub de Beer, 2019, 2020
                      Tom Verhoeff, 2019
   License      : None
   
   The "ExprLanguage" module defines and exports three things:

   1. The data type `Expr`
   2. The parser function `parseExpr` to translate strings to
      `Expr`s 
   3. The data type `ParseError` that is used when `parseExpr` fails. See
      [Parsec's Hackage page](https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html#t:ParseError)
      for more information about the `ParseError`.
-}
module ExprLanguage (
                      Expr(Var, Const, Plus, Mult)
                    , parseExpr
                    , ParseError
                    ) where

  import Control.Monad
  import Text.Parsec
  import Text.Parsec.String
  import Text.Parsec.Expr
  import qualified Text.Parsec.Token as Token
  import Text.Parsec.Language

  {-|
    = Expression data type

    The expression language defined in this module is very simple: it only
    contains variables and constants and two operators, addition and
    multiplication. 
  -}
  data Expr a b = 
                  Var a 
                | Const b 
                | Plus (Expr a b) (Expr a b) 
                | Mult (Expr a b) (Expr a b)
                deriving (Eq, Show)

  {-|
     The `Expr` type is polymorphic in the type of the variables and the
     type of the constants. Most often you will use String for
     variables and Integer or another number type for the constant
     values. For example:

     >>> anExpr :: Expr String Int
     >>> anExpr = Mult (Plus (Var "x") (Const (-1))) (Plus (Var "x") (Const 1))

     Do note that @-1@ needs to be parenthesized in Haskell.

    = Expression parser: Interface

    Writing values of `Expr` is painful because `Expr` values become large fast, even
    for relatively small expressions. Particularly compared to the same
    expressions written in mathematical notation, like \((x + (-1)) * (x +
    1)\). This module defines the function `parseExpr` to make
    creating and working with expressions easier.

    Given a String, `parseExpr` either returns a value of the type 
    @Either ParseError (Expr String Integer)@: if parsing was successful
    you get an expression with String variables and
    Integer constant values; if parsing was unsuccessful you get a
    `ParseError`.

    For example, the function

    @
    showOrFail :: String -> String
    showOrFail s = case (parseExpr s) of
                    Left  err   -> "Whoeps!"
                    Right expr  -> (show expr)
    @

    will return a String representation of the expression if parsing
    String s succeeded, otherwise it will return "Whoeps!".
  -}
  parseExpr :: String -> Either ParseError (Expr String Integer)
  parseExpr = parse (whiteSpace >> expr) "expression"


  {-|
     = Expression parser: Implementation

     You do not need to know the details of the parser, but if you are interested
     you can read on. 

     The parser is based on the example [Parsing a simple imperative
     language](https://wiki.haskell.org/Parsing_a_simple_imperative_language)
     on the Haskell wiki. It uses the
     [Parsec](https://hackage.haskell.org/package/parsec) library, in
     particular the expression-parser-generator
     [buildExpressionParser](https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec-Expr.html).

     The first step in generating the parser is to create a language definition for
     the token parser generator. The `EmptyDef` language is used as a
     basis and we configure it to accept identifiers for our variables and we
     reserve two operators, \(+\) and \(*\)
  -}
  exprLanguageDef = 
    emptyDef  { Token.identStart      = letter
              , Token.identLetter     = alphaNum
              , Token.reservedOpNames = ["*", "+"]
              }

  -- Using the exprLanguageDef Parsec can generate a lexer automatically:
  lexer = Token.makeTokenParser exprLanguageDef

  {-|
     To make using tokens easier, we define them at the level of the module. We will use identifiers, operators, parenthesis, integers, and whitespace.
  -}
  identifier = Token.identifier lexer
  reservedOp = Token.reservedOp lexer
  parens     = Token.parens     lexer
  integer    = Token.integer    lexer
  whiteSpace = Token.whiteSpace lexer

  {-|
     The actual parser is generated based on a definition of operators and the
     rules to recognize elements in the expression language called @term@
  -}
  expr :: Parser (Expr String Integer)
  expr = buildExpressionParser operators term

  operators = [ 
                [Infix (reservedOp "*" >> return (Mult)) AssocLeft]
              , [Infix (reservedOp "+" >> return (Plus)) AssocLeft]
              ]
              
  term   =   parens expr
         <|>  liftM Var identifier
         <|>  liftM Const integer

  {-|
     The operators are defined as a list of lists of operator definitions.
     Operators in the same list have the same priority. Operator lists that
     appear before another list do have a higher priority. In our simple
                                   language, we only have two infix binary
                                   operators, \(*\) and \(+\), the first of
                                   which should have higher priority.  

     This language is easy to extend: just add operators at the appropriate
     level.  However, if you do add another operator, do not forget to update
                         the `Expr` type with a constructor matching
                         that operator.
  -}
