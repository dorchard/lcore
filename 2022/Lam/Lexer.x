{
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Lam.Lexer (Token(..),scanTokens,symString
                 ,getPos) where

import Data.Text (Text)
import GHC.Generics (Generic)

}

%wrapper "posn"

$digit  = 0-9
$alpha  = [a-zA-Z\_\-\=]
$lower  = [a-z]
$upper  = [A-Z]
$eol    = [\n]
$alphanum  = [$alpha $digit \_]
@sym    = $lower ($alphanum | \')*
@constr = ($upper ($alphanum | \')* | \(\))
@int    = \-? $digit+
@charLiteral = \' ([\\.]|[^\']| . ) \'
@stringLiteral = \"(\\.|[^\"]|\n)*\"

@langPrag = [a-z]+

tokens :-

  $white*$eol                   { \p s -> TokenNL p }
  $eol+                         { \p s -> TokenNL p }
  $white+                       ;
  @constr                       { \p s -> TokenConstr p s }
  "--".*                        ;
  \\                            { \p s -> TokenLambda p }
  \=                            { \p s -> TokenEq p }
  \(                            { \p s -> TokenLParen p }
  \)                            { \p s -> TokenRParen p }
  -- Session 3 (lanugage options and typing)
  lang\.@langPrag               { \p s -> TokenLang p s }
  "->"                          { \p s -> TokenArrow p }
  "=>"                          { \p s -> TokenDoubleArrow p }
  ":"                           { \p s -> TokenColon p }
  -- Session 4 (PCF)
  "|"                           { \p s -> TokenSep p }
  case                          { \p s -> TokenCase p }
  of                            { \p s -> TokenOf p }
  fix                           { \p s -> TokenFix p }
  succ                          { \p s -> TokenSucc p }
  zero                          { \p s -> TokenZero p }
  fn                            { \p s -> TokenFun p }
  @sym				                  { \p s -> TokenSym p s }
  

{

data Token
  = TokenLambda   { posn :: AlexPosn }
  | TokenSym      { posn :: AlexPosn, sym :: String }
  | TokenEq       { posn :: AlexPosn }
  | TokenArrow    { posn :: AlexPosn }
  | TokenDoubleArrow { posn :: AlexPosn }
  | TokenFun      { posn :: AlexPosn }
  | TokenLParen   { posn :: AlexPosn }
  | TokenRParen   { posn :: AlexPosn }
  | TokenNL       { posn :: AlexPosn }
  | TokenLang     { posn :: AlexPosn, sym :: String }
  | TokenConstr   { posn :: AlexPosn, sym :: String }
  | TokenColon    { posn :: AlexPosn }
  | TokenCase     { posn :: AlexPosn }
  | TokenOf       { posn :: AlexPosn }
  | TokenSep      { posn :: AlexPosn }
  | TokenFix      { posn :: AlexPosn }
  | TokenSucc     { posn :: AlexPosn }
  | TokenZero     { posn :: AlexPosn }
  deriving (Eq, Show, Generic)

symString :: Token -> String
symString (TokenSym _ x) = x
symString t = error $ "Not a symbol " ++ show t

scanTokens = alexScanTokens >>= (return . trim)

trim :: [Token] -> [Token]
trim = reverse . trimNL . reverse . trimNL

trimNL :: [Token] -> [Token]
trimNL [] = []
trimNL (TokenNL _ : ts) = trimNL ts
trimNL ts = ts

getPos :: Token -> (Int, Int)
getPos t = (l, c)
  where (AlexPn _ l c) = posn t

}
