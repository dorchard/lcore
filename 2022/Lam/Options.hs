
module Lam.Options where

data Option = Typed | PCF
  deriving (Eq, Show)

isTyped :: [Option] -> Bool
isTyped options = elem Typed options

isPCF  :: [Option] -> Bool
isPCF options = elem PCF options