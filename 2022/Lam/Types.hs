module Lam.Types where

import Lam.Syntax
import Lam.PrettyPrint

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


-- checking
-- G |- t <= A
check :: Context -> Expr -> Type -> Bool
check = error "TODO"


-- inference
-- G |- t => A
synth :: Context -> Expr -> Maybe Type

{-

var ----------------------
       G, x : A |- x => A
-}
synth gamma (Var x) = lookup x gamma

{-

      G, x : A |- t => B
abs ------------------------
      G |- \x -> t => A -> B

-}
synth gamma (Abs x (Just tyA) t) =

  case synth ((x, tyA):gamma) t of
    Just tyB -> Just (FunTy tyA tyB)
    Nothing  -> Nothing

{-
     G |- t1 => A -> B      G |- t2 => A'    A == A'
app ------------------------------------------------
    G |- t1 t2 => B
-}
synth gamma (App t1 t2) =
  case synth gamma t1 of
    Just (FunTy tyA tyB) ->

      case synth gamma t2 of
        Just tyA' ->
          if tyA == tyA' -- type of argument matches type of function parameter
            then Just tyB
            else error $ pprint tyA ++ " does not match " ++ pprint tyA'
        Nothing -> Nothing

    Just _ -> error $ "Left hand side of application " ++ pprint t1 ++ " is not a function"
    Nothing -> Nothing

synth _ t = error $ "Cannot infer type of " ++ pprint t