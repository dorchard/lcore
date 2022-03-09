module Lam.Semantics where

import Lam.Syntax

-- reduce to normal form with number of reductions
multiStep :: Expr -> (Expr, Int)
multiStep t =
  case step t of
    Nothing -> (t, 0)
    Just t' -> let (t'', n) = multiStep t' in (t'', n + 1)

-- single step reduction
step :: Expr -> Maybe Expr
-- beta
step (App (Abs x t) t') = Just (subst t t' x)

-- appL
step (App t1 t2) =
  case step t1 of
    Just t1' -> Just (App t1' t2)
    Nothing  -> Nothing

-- everything else is a Call-By-Name normal form
step t = Nothing


-- subst t t' x  computes t[t'/x]
-- i.e., inside of t substitute every occurence of x with t'
subst :: Expr -> Expr -> Identifier -> Expr
subst (Var y) t' x | y == x    = t'
                   | otherwise = Var y
subst (App t1 t2) t' x         = App (subst t1 t' x) (subst t2 t' x)
-- TODO: avoid variable capture
subst (Abs y t)   t' x         = Abs y (subst t t' x)