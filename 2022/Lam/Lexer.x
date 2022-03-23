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
  "--".*                        ;
  @sym				                  { \p s -> TokenSym p s }
  "->"                          { \p s -> TokenArrow p }
  \\                            { \p s -> TokenLambda p }
  \=                            { \p s -> TokenEq p }
  \(                            { \p s -> TokenLParen p }
  \)                            { \p s -> TokenRParen p }
  lang\.@langPrag               { \p s -> TokenLang p s }
  
{

data Token
  = TokenLambda   { posn :: AlexPosn }
  | TokenSym      { posn :: AlexPosn, sym :: String }
  | TokenEq       { posn :: AlexPosn }
  | TokenArrow    { posn :: AlexPosn }
  | TokenLParen   { posn :: AlexPosn }
  | TokenRParen   { posn :: AlexPosn }
  | TokenNL       { posn :: AlexPosn }
  | TokenLang     { posn :: AlexPosn, sym :: String }
  | TokenConstr   { posn :: AlexPosn, sym :: String }
  deriving (Eq, Show, Generic)

symString :: Token -> String
symString (TokenSym _ x) = x
symString _ = error "Not a symbol"

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
