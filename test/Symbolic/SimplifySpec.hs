module Symbolic.SimplifySpec where

import Symbolic.Syntax
import Symbolic.Small as Small
import Symbolic.Big as Big
import Test.Hspec

spec :: Spec
spec = describe "simplification test" $ do
        let e = Add (Mult (Add (Const 1) (Mult (Const 0) (Var "x"))) (Const 3)) (Const 12)
        it "should work with small-step semantics" $ do
          Small.simplify e `shouldBe` Const 15
        it "should work with big-step semantics" $ do
          Big.simplify e `shouldBe` Const 15
