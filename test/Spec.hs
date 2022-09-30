{-|
Module      : FormulatorCLI
Description : The command-line interface for the formulator program
Copyright   : Mat Verhoeven (1728342)
              David Chen (1742477)

Testcases for:
    foldE, 
    printE, 
    evalE, 
    simplifyE, 
    diffE, 
    processCLIArgs.
-}

import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           ExprLanguage                   ( Expr(Var, Const, Plus, Mult), parseExpr )
import           FormulaManipulator             ( foldE
                                                , printE
                                                , evalE
                                                , simplifyE
                                                , diffE
                                                )
import           FormulatorCLI                  ( processCLIArgs )
import           Data.Either
naturals :: Gen Integer
naturals = choose (1, 10000)

helpmsg = "This is our Command Line Formula manipulator! \n\
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

main :: IO ()
main = hspec $ do
  describe "FormulaManipulator" $ do

    -- testcases for foldE
    describe "foldE" $ do
       it "should have the correct amount of folds" $ do
         foldE (const 1) (const 1) (+) (+) (Mult (Plus (Const 1) (Var "x")) (Plus (Const 1) (Var "x"))::Expr String Integer) 
         `shouldBe` (4 :: Integer)
       it "should distinguish between deconstructing Plus and Mult (Plus)" $ do
        foldE (const 3) (const 3) (+) (*) (Plus (Var "x") (Const 2) :: Expr String Integer) `shouldBe` (6 :: Int)
       it "should distinguish between deconstructing Plus and Mult (Mult)" $ do
        foldE (const 3) (const 3) (+) (*) (Mult (Var "x") (Const 2) :: Expr String Integer) `shouldBe` (9 :: Int)
       it "should be able to act on values in the Variable constructor" $ do
        foldE id show (++) (++) (Plus (Var "hello ") (Var "world") :: Expr String Integer) `shouldBe` ("hello world" :: String)
       it "should be able to act on values in the Constant constructor" $ do
        foldE id show (++) (++) (Plus (Const 3) (Const 4) :: Expr String Integer) `shouldBe` ("34" :: String)
       it "should be able to imitate the identity function" $ do
        foldE Var Const Plus Mult (Mult (Plus (Const 1) (Var "x")) (Plus (Const 1) (Var "x"))) --want this to be a forall, not sure how
        `shouldBe` (Mult (Plus (Const 1) (Var "x")) (Plus (Const 1) (Var "x")) :: Expr String Integer)
    
    -- testcases for printE
    describe "printE" $ do
      it "should convert Mult (Plus (Const 1) (Var \"x\")) (Plus (Const 1) (Var \"x\")) to \"(1+x)*(1+x)\"" $ do
        printE (Mult (Plus (Const 1) (Var "x")) (Plus (Const 1) (Var "x"))::Expr String Integer) 
        `shouldBe` ("(1 + x) * (1 + x)":: String)
      it "should convert Const 0 to \"0\"" $ do
        printE (Const 0::Expr String Integer) 
        `shouldBe` ("0":: String)
      it "should convert Plus (Const 2) (Const 2) to \"(2 + 2)\"" $ do
        printE (Plus (Const 2) (Const 2)::Expr String Integer) 
        `shouldBe` ("(2 + 2)":: String)
      it "should convert Mult (Const 2) (Const 2) to \"2 * 2\"" $ do
        printE (Mult (Const 2) (Const 2)::Expr String Integer) 
        `shouldBe` ("2 * 2":: String)
      it "should return the same expression when applying parseExpr . printE" $ do -- want this to be a forall
        (parseExpr . printE) (Mult (Plus (Const 1) (Var "x")) (Plus (Const 1) (Var "x"))::Expr String Integer) 
        `shouldBe` Right (Mult (Plus (Const 1) (Var "x")) (Plus (Const 1) (Var "x"))) 

    -- testcases for evalE
    describe "evalE" $ do
      it "x+1 for x=4 should equal 5" $ do
        evalE (\v -> if v == "x" then 4 else error "unknown variable") (Plus (Var "x") (Const 1) :: Expr String Integer) 
        `shouldBe` (5 :: Integer)
      it "1+1 should evaluate to 2" $ do
        evalE (\v -> error "There should be no variable") (Plus (Const 1) (Const 1) :: Expr String Integer) 
        `shouldBe` (2 :: Integer)
      it "5*5 should evaluate to 25" $ do
        evalE (\v -> error "There should be no variable") (Mult (Const 5) (Const 5) :: Expr String Integer) 
        `shouldBe` (25 :: Integer)
      it "should give an error when finding a variable not mapped to a value" $ do
        evaluate(evalE (\v -> if v == "x" then 4 else error "unknown variable") (Plus (Var "y") (Const 1) :: Expr String Integer)) 
        `shouldThrow` anyErrorCall      
      it "should evaluate x+x*y+1 for x = 1, y = 2 to 4" $ do
        evalE (\v -> if v == "x" then 1 else if v == "y" then 2 else error "unknown variable") (Plus (Var "x") (Plus (Mult (Var "x") (Var "y")) (Const 1)) :: Expr String Integer) 
        `shouldBe` (4 :: Integer)
      it "(forall n : n in N : it should evaluate x+3 with x = n to n + 3" $ property $
        forAll naturals (\n -> evalE (\v -> if v == "x" then n else error "unknown variable") (Plus (Var "x") (Const 3)) == n + 3)
      it "(forall n : n in N : it should evaluate x+x with x = n to 2*n" $ property $
        forAll naturals (\n -> evalE (\v -> if v == "x" then n else error "unknown variable") (Plus (Var "x") (Var "x")) == 2*n)


    -- testcases for simplifyE
    describe "simplifyE" $ do
      it "should evaluate 3*2 to 6" $ do
        (simplifyE (Mult (Const 3) (Const 2)) :: Expr String Int) 
        `shouldBe` (Const 6)
      it "should evaluate 3+2 to 5" $ do
        (simplifyE (Plus (Const 3) (Const 2)) :: Expr String Int) 
        `shouldBe` (Const 5)
      it "should evaluate x+0 to x" $ do
        (simplifyE (Plus (Var "x") (Const 0)) :: Expr String Int) 
        `shouldBe` (Var "x")
      it "should evaluate 0+x to x" $ do
        (simplifyE (Plus (Const 0) (Var "x")) :: Expr String Int) 
        `shouldBe` (Var "x")
      it "should evaluate x*0 to 0" $ do
        (simplifyE (Mult (Var "x") (Const 0) ) :: Expr String Int) 
        `shouldBe` (Const 0)
      it "should evaluate 0*x to 0" $ do
        (simplifyE (Mult (Const 0) (Var "x")) :: Expr String Int) 
        `shouldBe` (Const 0)
      it "should evaluate x*1 to x" $ do
        (simplifyE (Mult (Var "x") (Const 1)) :: Expr String Int) 
        `shouldBe` (Var "x")
      it "should evaluate 1*x to x" $ do
        (simplifyE (Mult (Const 1) (Var "x")) :: Expr String Int) 
        `shouldBe` (Var "x")
      it "should evaluate x+2 to x+2" $ do
        (simplifyE (Plus (Var "x") (Const 2)) :: Expr String Int) 
        `shouldBe` (Plus (Var "x") (Const 2))
      it "should evaluate x*0+x*2+0 to x*2" $ do
        ((simplifyE . fromRight (Var "error") . parseExpr) "x*0+x*2+0":: Expr String Integer) 
        `shouldBe` (Mult (Var "x") (Const 2))
      it "should evaluate (x*y)*0 to 0" $ do
        ((simplifyE . fromRight (Var "error") . parseExpr) "(x*y)*0":: Expr String Integer) 
        `shouldBe` (Const 0)              
      it "should evaluate x*(y*0) to 0" $ do
        ((simplifyE . fromRight (Var "error") . parseExpr) "x*(y*0)":: Expr String Integer) 
        `shouldBe` (Const 0)             
--      it "(forall n : n in N : (r2i . i2r) n >= 1)" $ property $
--        forAll naturals (\n -> (r2i . i2r) n >= (1::Int))
      


    -- testcases for diffE
    describe "diffE" $ do
      it "should differentiate x to 1" $ do
        (diffE "x" (Var "x") :: Expr String Integer) 
        `shouldBe` (Const 1)   
      it "should differentiate a constant to 0" $ property $
        forAll naturals (\n -> diffE "x" (Const n) == (Const 0))
      it "should differentiate z to 0 w.r.t. x" $ do
        (diffE "x" (Var "z") :: Expr String Integer) 
        `shouldBe` (Const 0)    
      it "should differentiate z*x to z w.r.t. x" $ do
        ((simplifyE . (diffE "x")) (Mult (Var "z") (Var "x")) :: Expr String Integer) 
        `shouldBe` (Var "z")                
      it "should differentiate 3*x + 2*x to 5 w.r.t. x" $ do
        ((simplifyE . (diffE "x") . fromRight (Mult (Var "Error") (Var "x")). parseExpr) "3*x + 2*x" :: Expr String Integer) 
        `shouldBe` (Const 5)      
      it "should differentiate (3*x) * (2*x) to 3*x*2 + 3*x*2 w.r.t. x" $ do
        ((simplifyE . (diffE "x") . fromRight (Mult (Var "Error") (Var "x")). parseExpr) "(3*x) * (2*x)" :: Expr String Integer) 
        `shouldBe` Plus (Mult (Const 3) (Mult (Const 2) (Var "x"))) (Mult (Mult (Const 3) (Var "x")) (Const 2))            
      it "let f = x*x + 1 + x, then f'= 2*x + 1" $ do
        (printE . simplifyE) (diffE "x" (Plus (Mult (Var "x") (Var "x")) (Plus (Const 1) (Var "x")) :: Expr String Integer ))
        `shouldBe` ("((x + x) + 1)" :: String)

  -- testcases for ...
  describe "FormulatorCLI" $ do
    describe "processCLIArgs" $ do
      it "should apply the print command when called with \"-p\"" $ do
        processCLIArgs ["-p", "(x+2)"] `shouldBe` "(x + 2)"
      it "should apply the print command when called with \"--print\"" $ do
        processCLIArgs ["--print", "(x+2)"] `shouldBe` "(x + 2)"        
      it "should apply the simplify command when called with \"-s\"" $ do
        processCLIArgs ["-s", "(x+2*5)"] `shouldBe` "(x + 10)"        
      it "should apply the simplify command when called with \"--simplify\"" $ do
        processCLIArgs ["--simplify", "(x+2*5)"] `shouldBe` "(x + 10)"  
      it "should apply the differentiate command when called with \"-d\"" $ do
        processCLIArgs ["-d", "x", "(x+2*5)"] `shouldBe` "1"  
      it "should apply the differentiate command when called with \"--differentiate\"" $ do
        processCLIArgs ["--differentiate", "x", "(x+2*5)"] `shouldBe` "1"  
      it "should apply the evaluate command when called with \"-e\"" $ do
        processCLIArgs ["-e","x=2;", "(x+2*5)"] `shouldBe` "12"  
      it "should apply the evaluate command when called with \"--evaluate\"" $ do
        processCLIArgs ["--evaluate","x=2;", "(x+2*5)"] `shouldBe` "12"
      it "should give a help message when called with \"-h\"" $ do
        processCLIArgs ["-h"] `shouldBe` helpmsg
      it "should give a help message when called with \"--help\"" $ do
        processCLIArgs ["--help"] `shouldBe` helpmsg      
      it "should give an error message when called with nothing" $ do
        processCLIArgs [] `shouldBe` "Please input command or type \"-h\" for help"
      it "should give an error message when an invalid command is given" $ do
        processCLIArgs ["-a"] `shouldBe` "Command not recognized, type \"-h\" for a help message."   
      it "should give an error message when print is called with nothing" $ do
        processCLIArgs ["-p"] `shouldBe` "No expression to print."
      it "should give an error message when simplify is called with nothing" $ do
        processCLIArgs ["-s"] `shouldBe` "No expression to simplify."
      it "should give an error message when evaluate is called with nothing" $ do
        processCLIArgs ["-e"] `shouldBe` "No expression to evaluate, no variable-to-value dictionary."    
      it "should give an error message when evaluate is called with not enough arguments" $ do
        processCLIArgs ["-e", "x=2;"] `shouldBe` "No expression to evaluate."     
      it "should give an error message when evaluate is called with not variables being mapped" $ do
        evaluate (processCLIArgs ["-e", "x=2;", "x+y"]) `shouldThrow` anyErrorCall  
      it "should give an error message when differentiate is called with nothing" $ do
        processCLIArgs ["-d"] `shouldBe` "No expression to derivate, no variable to take the derivative to."
      it "should give an error message when differentiate is called with nothing" $ do
        processCLIArgs ["-d", "x"] `shouldBe` "No expression to derivate."

