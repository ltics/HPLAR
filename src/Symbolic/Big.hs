module Symbolic.Big where

import Symbolic.Syntax

simplify :: Expr -> Expr
simplify e = case e of
               Add (Const 0) x -> simplify x
               Add x (Const 0) -> simplify x
               Mult x (Const 0) -> Const 0
               Mult (Const 0) x -> Const 0
               Mult x (Const 1) -> simplify x
               Mult (Const 1) x -> simplify x
               Add (Const a) (Const b) -> Const $ a + b
               Mult (Const a) (Const b) -> Const $ a * b
               Add e1 e2 -> simplify $ Add (simplify e1) (simplify e2)
               Mult e1 e2 -> simplify $ Mult (simplify e1) (simplify e2)
               _ -> e