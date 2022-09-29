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
        `shouldBe` ("(1+x)*(1+x)":: String)
      it "should convert Const 0 to \"0\"" $ do
        printE (Const 0::Expr String Integer) 
        `shouldBe` ("0":: String)
      it "should convert Plus (Const 2) (Const 2) to \"(2+2)\"" $ do
        printE (Plus (Const 2) (Const 2)::Expr String Integer) 
        `shouldBe` ("(2+2)":: String)
      it "should convert Mult (Const 2) (Const 2) to \"2*2\"" $ do
        printE (Mult (Const 2) (Const 2)::Expr String Integer) 
        `shouldBe` ("2*2":: String)
      it "should return the same expression when applying parseExpr . printE" $ do -- want this to be a forall
        (parseExpr . printE) (Mult (Plus (Const 1) (Var "x")) (Plus (Const 1) (Var "x"))::Expr String Integer) 
        `shouldBe` Right (Mult (Plus (Const 1) (Var "x")) (Plus (Const 1) (Var "x"))) 

    -- -- testcases for evalE
    -- describe "evalE" $ do
    --   it "should have tests" $ do
    --     (1 :: Integer) `shouldBe` (1 :: Integer)

    -- -- testcases for simplifyE
    -- describe "simplifyE" $ do
    --   it "should have tests" $ do
    --     (1 :: Integer) `shouldBe` (1 :: Integer)

    -- testcases for diffE
    describe "diffE" $ do
      it "let f = x*x + 1 + x, then f'= 2*x + 1" $ do
        (printE . simplifyE) (diffE "x" (Plus (Mult (Var "x") (Var "x")) (Plus (Const 1) (Var "x")):: Expr String Integer ))
        `shouldBe` ("((x+x)+1)" :: String)

  -- -- testcases for ...
  -- describe "FormulatorCLI" $ do
  --   describe "processCLIArgs" $ do
  --     it "should have tests" $ do
  --       (1 :: Integer) `shouldBe` (1 :: Integer)

