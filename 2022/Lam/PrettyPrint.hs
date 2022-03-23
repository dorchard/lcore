{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Lam.PrettyPrint where

import Lam.Syntax

-- Pretty print terms
class PrettyPrint t where
    isLexicallyAtomic :: t -> Bool
    isLexicallyAtomic _ = False

    pprint :: t -> String

bracket_pprint :: PrettyPrint t => t -> String
bracket_pprint t | isLexicallyAtomic t = pprint t
                 | otherwise           = "(" ++ pprint t ++ ")"

-- Untyped lambda calculus
instance PrettyPrint Expr where
    isLexicallyAtomic (Var _)     = True
    isLexicallyAtomic _           = False

    pprint (Abs var Nothing e) = "\\" ++ var ++ " -> " ++ pprint e ++ ""
    pprint (Abs var (Just ty) e) = "\\(" ++ var ++ ":" ++ pprint ty ++ ") -> " ++ pprint e ++ ""
    
    pprint (App e1 e2) = bracket_pprint e1 ++ " " ++ bracket_pprint e2
    pprint (Var var)   = var

instance PrettyPrint Type where
    isLexicallyAtomic (Cons n) = True
    isLexicallyAtomic _        = False

    pprint (FunTy t1 t2) = bracket_pprint t1 ++ " -> " ++ bracket_pprint t2
    pprint (Cons n)      = n