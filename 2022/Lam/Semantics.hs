{-# LANGUAGE ImplicitParams #-}

module Lam.Semantics where

import Data.List (nub, delete)
import Lam.Syntax
import Lam.Options

-- reduce to normal form with number of reductions
multiStep :: (?options :: [Option]) => Expr -> (Expr, Int)
multiStep t =
  let step' = if CBV `elem` ?options then stepCBV else stepCBN in
  case step' t of
    Nothing -> (t, 0)
    Just t' ->
      let (t'', n) = multiStep t' in (t'', n + 1)


-- single step reduction - call-by-name
stepCBN :: Expr -> Maybe Expr

--
--   --------------------------- beta
--    (\x . t) t'  ~>  t[t'/x]

stepCBN (App (Abs x _ t) t') = Just (subst t t' x)

stepCBN (App t1 t2) =

--           t1 ~> t1'
--   --------------------------- appL
--       t1 t2  ~>  t1' t2

  case stepCBN t1 of
    Just t1' -> Just (App t1' t2)
    Nothing  ->

-- Not necessary for call-by-name but useful

--           t2 ~> t2'
--   --------------------------- appR
--       v t2  ~>  v t2'

      case stepCBN t2 of
        Just t2' -> Just (App t1 t2')
        Nothing  -> Nothing

--             t ~> t'
--   --------------------------- abs
--       \y . t  ~>  \y . t'

stepCBN (Abs y mty t) =
  case stepCBN t of
    Just t' -> Just (Abs y mty t')
    Nothing -> Nothing

-- PCF SEMANTICS

stepCBN (Succ t) =
  case stepCBN t of
    Just t' -> Just (Succ t')
    Nothing -> Nothing

stepCBN (Fix t) =
  Just (App t (Fix t))

stepCBN (Case Zero t1 (y, t2)) =
  Just t1

stepCBN (Case (Succ t) t1 (y, t2)) =
  Just $ subst t2 t y

stepCBN (Case t t1 (y, t2)) =
  case stepCBN t of
    Just t' -> Just $ Case t' t1 (y, t2)
    Nothing -> Nothing

-- everything else is a normal form
stepCBN _ = Nothing


-- single step reduction - call-by-value
stepCBV :: Expr -> Maybe Expr
stepCBV (App (Abs x ty t) t2) = 
  case stepCBV t2 of
    Just t2' -> Just (App (Abs x ty t) t2')
    Nothing  -> Just (subst t t2 x)

stepCBV (App t1 t2) = 
  case stepCBV t1 of
    Just t1' -> Just (App t1' t2)
    Nothing ->
      case stepCBV t2 of
        Just t2' -> Just (App t1 t2')
        Nothing  -> Nothing

stepCBV (Abs x ty t) =
  case stepCBV t of
    Just t' -> Just (Abs x ty t')
    Nothing -> Nothing

stepCBV _ = Nothing


-- subst t t' x  computes t[t'/x]
-- i.e., inside of t substitute every occurence of x with t'
subst :: Expr -> Expr -> Identifier -> Expr
subst (Var y) t' x | y == x    = t'
                   | otherwise = Var y
subst (App t1 t2) t' x         = App (subst t1 t' x) (subst t2 t' x)

subst (Abs y mty t)   t' x =
  Abs y' mty t''
  where
    (y', t'') = substituteUnderBinder (y, t) t' x

subst Zero t' x     = Zero
subst (Succ t) t' x = Succ (subst t t' x)
subst (Fix t) t' x  = Fix (subst t t' x)
subst (Case t t1 (y, t2)) t' x =
  Case (subst t t' x) (subst t1 t' x) (substituteUnderBinder (y, t2) t' x)
subst (Sig t ty) t' x = Sig (subst t t' x) ty


-- substituteUnderBinder (y, t) t' x = (y', t'')
--  substitutes t' into t to yield t'' where y is a binder in t which gets freshened to y'
substituteUnderBinder :: (Identifier, Expr) -> Expr -> Identifier -> (Identifier, Expr)
substituteUnderBinder (y, t) t' x =
  if x == y
    then (y, t)
    else
      if y `elem` freeVars t'
        then
          let freshVar = y ++ "'"
              -- First freshen y intside of t
              freshened = subst t (Var freshVar) y
          -- then apply the substitution, capture has been avoided
          in (freshVar, subst freshened t' x)

        else (y, subst t t' x)

freeVars :: Expr -> [Identifier]
freeVars (Var x)     = [x]
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2
freeVars (Abs x _ t)   = deleteAll x (freeVars t)
  where deleteAll x xs = delete x (nub xs)
-- PCF
freeVars (Fix t)             = freeVars t
freeVars (Case t t1 (x, t2)) = freeVars t ++ freeVars t1 ++ (delete x (freeVars t2))
freeVars Zero                = []
freeVars (Succ t)            = freeVars t
freeVars (Sig t _)           = freeVars t