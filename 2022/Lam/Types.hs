module Lam.Types where

import Lam.Syntax

{-

*************************************************************
Declarative specification of the simply-typed lambda calculus
*************************************************************
Recall contexts are like lists of variable-type assumptions

G ::=  G, x : A | .

       (x : A) in G
var ----------------------
       G |- x : A

     G |- e1 : A -> B      G |- e2 : A
app ---------------------------------------
    G |- e1 e2 : B

      G, x : A |- e : B
abs ------------------------
      G |- \x -> e : A -> B

-}

-- Represent contexts as lists
type Context = [(Identifier, Type)]

type Name = String

data Type = FunTy Type Type | Cons Name
  deriving (Show, Eq)

check :: Context -> Expr -> Type -> Bool
check = error "TODO"

synth :: Context -> Expr -> Maybe Type
synth = error "TODO"