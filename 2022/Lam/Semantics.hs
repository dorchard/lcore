module Lam.Semantics where

import Data.List (nub, delete)
import Lam.Syntax

-- reduce to normal form with number of reductions
multiStep :: Expr -> (Expr, Int)
multiStep t =
  case step t of
    Nothing -> (t, 0)
    Just t' -> let (t'', n) = multiStep t' in (t'', n + 1)

-- single step reduction
step :: Expr -> Maybe Expr

--
--   --------------------------- beta
--    (\x . t) t'  ~>  t[t'/x]

step (App (Abs x t) t') = Just (subst t t' x)

step (App t1 t2) =

--           t1 ~> t1'
--   --------------------------- appL
--       t1 t2  ~>  t1' t2

  case step t1 of
    Just t1' -> Just (App t1' t2)
    Nothing  ->

--           t2 ~> t2'
--   --------------------------- appR
--       t1 t2  ~>  t1 t2'

      case step t2 of
        Just t2' -> Just (App t1 t2')
        Nothing  -> Nothing

--             t ~> t'
--   --------------------------- abs
--       \y . t  ~>  \y . t'

step (Abs y t) =
  case step t of
    Just t' -> Just (Abs y t')
    Nothing -> Nothing

-- everything else is a normal form
step _ = Nothing



-- subst t t' x  computes t[t'/x]
-- i.e., inside of t substitute every occurence of x with t'
subst :: Expr -> Expr -> Identifier -> Expr
subst (Var y) t' x | y == x    = t'
                   | otherwise = Var y
subst (App t1 t2) t' x         = App (subst t1 t' x) (subst t2 t' x)

subst (Abs y t)   t' x =
  if x == y
    then Abs y t
    else
      if y `elem` freeVars t'
        then
          let freshVar = y ++ "'"
              -- First freshen y intside of t
              freshened = subst t (Var freshVar) y
          -- then apply the substitution, capture has been avoided
          in Abs freshVar (subst freshened t' x)

        else Abs y (subst t t' x)

freeVars :: Expr -> [Identifier]
freeVars (Var x)     = [x]
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2
freeVars (Abs x t)   = deleteAll x (freeVars t)
  where deleteAll x xs = delete x (nub xs)

-- freeVars (x y) = [x, y]
-- freeVars (\x -> x y) = [y]

-- Problems with the above
--
--   Replacing bound variable:
--     subst (\y . y) t y        = \y . t
--   WHICH IS WRONG

--   Free variable now 'captured' by bound variable
--     subst (\x . \y . x z) (y y) z = \x . \y . x (y y)
--   WHICH IS WRONG