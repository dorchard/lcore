module Lam.Syntax where

type Identifier = String

-- Lambda calculus AST
data Expr =
    Var Identifier            -- x, y
  | App Expr Expr             -- t1 t2
  | Abs Identifier (Maybe Type) Expr  -- \x . t  or \(x : A) . t
  deriving Show

-- Syntax of types
type Name = String

data Type = FunTy Type Type  -- t1 -> t2
          | Cons Name        -- A, B
  deriving (Show, Eq)