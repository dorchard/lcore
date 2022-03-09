{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Lam.Syntax where

type Identifier = String

-- Abstract-syntax tree for LambdaCore
data Expr where
    Abs :: Identifier -> Expr -> Expr -- \x -> e  [λ x . e]
    App :: Expr       -> Expr -> Expr -- e1 e2
    Var :: Identifier         -> Expr -- x
  deriving Show
