{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}

module Lam.PrettyPrint where

import Lam.Options
import Lam.Syntax

-- Pretty print terms
class PrettyPrint t where
    isLexicallyAtomic :: t -> Bool
    isLexicallyAtomic _ = False

    pprint :: (?options :: [Option]) => t -> String

bracket_pprint :: (?options :: [Option], PrettyPrint t) => t -> String
bracket_pprint t | isLexicallyAtomic t = pprint t
                 | otherwise           = "(" <> pprint t <> ")"

-- Untyped lambda calculus
instance PrettyPrint Expr where
    isLexicallyAtomic (Var _)     = True
    isLexicallyAtomic Zero        = True
    isLexicallyAtomic _           = False

    pprint (Abs var Nothing e) = lamNotation <> " " <> var <> " " <> arrowNotation <> " " <> pprint e <> ""
    pprint (Abs var (Just ty) e) = lamNotation <> " " <> var <> ":" <> pprint ty <> ") " <> arrowNotation <> " " <> pprint e <> ""
    
    pprint (App e1 e2) = bracket_pprint e1 <> " " <> bracket_pprint e2
    pprint (Var var)   = var

    pprint (Fix e) = "fix " <> bracket_pprint e
    pprint (Case e e1 (x, e2)) = "case " <> bracket_pprint e <> " of "
                             <> " zero -> " <> bracket_pprint e1 <> " | "
                             <> " succ " <> x <> " -> " <> bracket_pprint e2
    pprint Zero     = "zero"
    pprint (Succ e) = "succ " <> bracket_pprint e
    pprint (Sig e t) = bracket_pprint e <> " : " <> pprint t


lamNotation :: (?options :: [Option]) => String
lamNotation = 
    if isSMLsyntax ?options then "fn" else "\\"

arrowNotation :: (?options :: [Option]) => String
arrowNotation =
    if isSMLsyntax ?options then "=>" else "->"

instance PrettyPrint Type where
    isLexicallyAtomic (Cons n) = True
    isLexicallyAtomic _        = False

    pprint (FunTy t1 t2) = bracket_pprint t1 <> " -> " <> bracket_pprint t2
    pprint (Cons n)      = n