
module Lam.Options where

data Option = Typed
  deriving (Eq, Show)

isTyped :: [Option] -> Bool
isTyped options = elem Typed options