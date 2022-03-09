{
{-# LANGUAGE FlexibleContexts #-}

module Lam.Parser where

import Numeric
import System.Exit
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

import Lam.Lexer
import Lam.Syntax

}

%name program Program
%name expr Expr
%tokentype { Token }
%error { parseError }
%monad { ReaderT String (Either String) }

%token
    nl      { TokenNL _ }
    '\\'    { TokenLambda _ }
    '->'    { TokenArrow _ }
    VAR     { TokenSym _ _ }
    '='     { TokenEq _ }
    '('     { TokenLParen _ }
    ')'     { TokenRParen _ }
    
    

%right '->'
%%

Program :: { Expr }
  : Defs  { $1 }

Defs :: { Expr }
  : Def NL Defs           { $1 $3 }
  | Expr                  { $1 }

NL :: { () }
  : nl NL                     { }
  | nl                        { }

Def :: { Expr -> Expr }
  : VAR '=' Expr { \program -> App (Abs (symString $1) program) $3 }

Expr :: { Expr }
  : '\\' VAR '->' Expr
    { Abs (symString $2) $4 }

  | Juxt
    { $1 }

Juxt :: { Expr }
  : Juxt Atom                 { App $1 $2 }
  | Atom                      { $1 }

Atom :: { Expr }
  : '(' Expr ')'              { $2 }
  | VAR                       { Var $ symString $1 }
  
{

parseError :: [Token] -> ReaderT String (Either String) a
parseError [] = lift . Left $ "Premature end of file"
parseError t  =  do
    file <- ask
    lift . Left $ file <> ":" <> show l <> ":" <> show c
                        <> ": parse error"
  where (l, c) = getPos (head t)

parseProgram :: FilePath -> String -> Either String Expr
parseProgram file input = runReaderT (program $ scanTokens input) file

}
