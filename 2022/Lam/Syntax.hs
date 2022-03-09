module Lam.Syntax where

type Identifier = String

-- Lambda calculus AST
data Expr =
    Var Identifier            -- x, y
  | App Expr Expr             -- t1 t2
  | Abs Identifier Expr       -- \x . t
  deriving Show


