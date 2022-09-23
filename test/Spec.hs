import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           ExprLanguage                   ( Expr(Var, Const, Plus, Mult) )
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

    -- -- testcases for foldE
    -- describe "foldE" $ do
    --   it "should have tests" $ do
    --     (1 :: Integer) `shouldBe` (1 :: Integer)
    -- ik heb eigenlijk geen idee hoe ik deze functie moet testen ?!

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
        

    -- -- testcases for evalE
    -- describe "evalE" $ do
    --   it "should have tests" $ do
    --     (1 :: Integer) `shouldBe` (1 :: Integer)

    -- -- testcases for simplifyE
    -- describe "simplifyE" $ do
    --   it "should have tests" $ do
    --     (1 :: Integer) `shouldBe` (1 :: Integer)

  --   -- testcases for diffE
  --   describe "diffE" $ do
  --     it "should have tests" $ do
  --       (1 :: Integer) `shouldBe` (1 :: Integer)

  -- -- testcases for ...
  -- describe "FormulatorCLI" $ do
  --   describe "processCLIArgs" $ do
  --     it "should have tests" $ do
  --       (1 :: Integer) `shouldBe` (1 :: Integer)

