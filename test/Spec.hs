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

    describe "foldE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "printE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "evalE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "simplifyE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

    describe "diffE" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

  describe "FormulatorCLI" $ do
    describe "processCLIArgs" $ do
      it "should have tests" $ do
        (1 :: Integer) `shouldBe` (1 :: Integer)

