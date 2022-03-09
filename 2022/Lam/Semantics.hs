module Lam.Semantics where

import Lam.Syntax

-- reduce to normal form with number of reductions
multiStep :: Expr -> (Expr, Int)
multiStep = error "TODO"

-- single step reduction
step :: Expr -> Maybe Expr
step = error "TODO"
