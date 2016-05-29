module Symbolic.Syntax where

data Expr = Var String
          | Const Int
          | Add Expr Expr
          | Mult Expr Expr
          deriving (Show, Eq)
