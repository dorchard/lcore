{
{-# LANGUAGE FlexibleContexts #-}

module Lam.Parser where

import Numeric
import System.Exit
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Lam.Lexer
import Lam.Syntax
import Lam.Options


}

%name program Program
%name expr Expr
%tokentype { Token }
%error { parseError }
%monad { StateT [Option] (ReaderT String (Either String)) }

%token
    nl      { TokenNL _ }
    '\\'    { TokenLambda _ }
    '->'    { TokenArrow _ }
    VAR     { TokenSym _ _ }
    '='     { TokenEq _ }
    '('     { TokenLParen _ }
    ')'     { TokenRParen _ }
    LANG    { TokenLang _ _ }

%right '->'
%%

Program :: { Expr }
  : LangOpts Defs  { $2 }

LangOpts :: { () }
  : LANG NL LangOpts    {% (readOption $1) >>= addOption }
  | LANG                {% readOption $1 >>= addOption }
  | {- empty -}         { () }


Defs :: { Expr }
  : Def NL Defs           { $1 $3 }
  | Expr                  { $1 }

NL :: { () }
  : nl NL                     { }
  | nl                        { }

Def :: { Expr -> Expr }
  : VAR '=' Expr { \program -> App (Abs (symString $1) program) $3 }

Expr :: { Expr }
  : '\\' VAR '->' Expr        { Abs (symString $2) $4 }

  | Juxt
    { $1 }

Juxt :: { Expr }
  : Juxt Atom                 { App $1 $2 }
  | Atom                      { $1 }

Atom :: { Expr }
  : '(' Expr ')'              { $2 }
  | VAR                       { Var $ symString $1 }
  
{
type ParserMonad a = StateT [Option] (ReaderT String (Either String)) a


readOption :: Token -> ParserMonad Option
readOption (TokenLang _ x) | x == "lang.typed" = return Typed
readOption (TokenLang _ x) = lift . lift . Left $ "Unknown language option: " <> x
readOption _ = lift . lift . Left $ "Wrong token for language"

addOption :: Option -> ParserMonad ()
addOption opt = do
  opts <- get
  put $ opt : opts

parseError :: [Token] -> ParserMonad a
parseError [] = lift . lift . Left $ "Premature end of file"
parseError t  =  do
    file <- lift $ ask
    lift . lift . Left $ file <> ":" <> show l <> ":" <> show c
                        <> ": parse error"
  where (l, c) = getPos (head t)

parseProgram :: FilePath -> String -> Either String (Expr, [Option])
parseProgram file input = runReaderT (runStateT (program $ scanTokens input) []) file

}
