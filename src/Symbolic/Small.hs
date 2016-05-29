module Symbolic.Small where

import Symbolic.Syntax

simplify1 :: Expr -> Expr
simplify1 e = case e of
                Add (Const 0) x           -> x
                Add x (Const 0)           -> x
                Add (Const a) (Const b)   -> Const $ a + b
                Mult x (Const 0)          -> Const 0
                Mult (Const 0) x          -> Const 0
                Mult x (Const 1)          -> x
                Mult (Const 1) x          -> x
                Mult (Const a) (Const b)  -> Const $ a * b
                _                         -> e

simplify :: Expr -> Expr
simplify e = case e of
               Add e1 e2   -> simplify1 $ Add (simplify e1) (simplify e2)
               Mult e1 e2  -> simplify1 $ Mult (simplify e1) (simplify e2)
               _           -> simplify1 e
