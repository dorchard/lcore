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

type TypeError = String



-- checking
-- G |- t <= A
check :: Context -> Expr -> Type -> Either TypeError Bool

{-

       G, x : A |- t <= B
abs ------------------------
      G |- \x -> t <= A -> B

-}

check gamma (Abs x _ t) (FunTy tyA tyB) =
  check ((x, tyA):gamma) t tyB

{-

    G |- e => A'    A == A'
 ---------------------------
    G |- e <= A

-}

-- Fall through
check gamma e t =
  case synth gamma e of
    Right t'   -> Right $ t == t'
    Left error -> Left error

-- inference
-- G |- t => A
synth :: Context -> Expr -> Either TypeError Type

{-

var ----------------------
       G, x : A |- x => A
-}
synth gamma (Var x) =
  case lookup x gamma of
    Just ty -> Right ty
    Nothing -> Left $ "I don't know the type of free variable " ++ x

{-

      G, x : A |- t => B
abs ------------------------
      G |- \(x:A) -> t => A -> B

-}
synth gamma (Abs x (Just tyA) t) =

  case synth ((x, tyA):gamma) t of
    Right tyB -> Right (FunTy tyA tyB)
    Left  err -> Left err

{-
     G |- t1 => A -> B      G |- t2 <= A
app ------------------------------------------------
           G |- t1 t2 => B
-}
synth gamma (App t1 t2) =
  case synth gamma t1 of
    Right (FunTy tyA tyB) ->

      case check gamma t2 tyA of
        Right True  -> Right tyB
        Right False -> Left $ "Expected type " ++ pprint tyA
        Left  err   -> Left err

    Right _ -> Left $ "Left hand side of application " ++ pprint t1 ++ " is not a function"
    Left err -> Left err


synth gamma Zero =
  Right (Cons "Nat")

synth gamma (Succ t) =
  case check gamma t (Cons "Nat") of
    Right True -> Right (Cons "Nat")
    Right False -> Left $ "Expected type Nat"
    Left err -> Left err

synth gamma (Case t t1 (y, t2)) =
 case check gamma t (Cons "Nat") of
   Right True ->
     case synth gamma t1 of
       Right tyA ->
         case check ((y,Cons "Nat"):gamma) t2 tyA of
           Right True -> return tyA
           _ -> undefined
       _ -> undefined
   _ -> undefined

synth _ t = Left $ "Cannot infer type of `" ++ pprint t ++ "`. Add more type signatures."