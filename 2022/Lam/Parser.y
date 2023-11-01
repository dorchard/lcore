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
    '=>'    { TokenDoubleArrow _ }
    '='     { TokenEq _ }
    ':'     { TokenColon _ }
    '('     { TokenLParen _ }
    ')'     { TokenRParen _ }
    LANG    { TokenLang _ _ }
    case    { TokenCase _ }
    fn      { TokenFun _ }
    of      { TokenOf _ }
    fix     { TokenFix _ }
    zero    { TokenZero _ }
    succ    { TokenSucc _ }
    '|'     { TokenSep _ }
    VAR     { TokenSym _ _ }
    CONSTR  { TokenConstr _ _ }

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
  : Variable '=' Expr { \program -> App (Abs $1 Nothing program) $3 }

Expr :: { Expr }
  : FunStart VAR FunArrow Expr        { Abs (symString $2) Nothing $4 }

  | FunStart '(' VAR ':' Type ')' FunArrow Expr
                              { Abs (symString $3) (Just $5) $8 }
  | Juxt
    { $1 }

  | fix Atom                  {% ifM isPCFM (return $ Fix $2) (return $ App (Var "fix") $2) }

  | case Expr of zero '->' Expr '|' succ VAR '->' Expr
                              {% ifM isPCFM
                                    (return $ Case $2 $6 (symString $9, $11))
                                    (error "`case` doesn't exist in the lambda calculus") }

  | succ Atom                 {% ifM isPCFM (return $ Succ $2) (return $ App (Var "succ") $2) }
  | Expr ':' Type             { Sig $1 $3 }

Type :: { Type }
  : Type '->' Type            { FunTy $1 $3 }
  | CONSTR                    { Cons (sym $1) }

Juxt :: { Expr }
  : Juxt Atom                 { App $1 $2 }
  | Atom                      { $1 }

Atom :: { Expr }
  : '(' Expr ')'              { $2 }
  | VAR                       { Var (symString $1) }
  | zero                      {% ifM isPCFM (return Zero) (return $ Var "zero") }

Variable :: { String }
 : VAR  { symString $1 }
 | zero { "zero" }
 | succ { "succ" }

FunArrow :: { () }
  : '->' { () }
  | '=>' {% allowSMLsyntax "=>" }

FunStart :: { () }
  : '\\'  { () }
  | fn    {% allowSMLsyntax "fn" }


{
type ParserMonad a = StateT [Option] (ReaderT String (Either String)) a

isPCFM :: ParserMonad Bool
isPCFM = get >>= (return . isPCF)

allowSMLsyntax :: String -> ParserMonad ()
allowSMLsyntax token =  do
  flag <- get
  if isSMLsyntax flag
    then (return ())
    else (error $ "Unexpected `" <> token <> "`; Perhaps you want to include lang.sml-syntax?")

readOption :: Token -> ParserMonad Option
readOption (TokenLang _ x) | x == "lang.typed" = return Typed
readOption (TokenLang _ x) | x == "lang.pcf"   = return PCF
readOption (TokenLang _ x) | x == "lang.cbv"   = return CBV
readOption (TokenLang _ x) | x == "lang.syntax"   = return SMLSyntax
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
                        <> ": parse error (token is " <> (show $ head t) <> ")"
  where (l, c) = getPos (head t)

parseProgram :: FilePath -> String -> Either String (Expr, [Option])
parseProgram file input = runReaderT (runStateT (program $ scanTokens input) []) file

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM condM left right = do
  cond <- condM
  if cond then left else right

}
