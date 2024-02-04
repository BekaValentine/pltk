module CSML.SExp where

data SExp
    = SExp String [SExp]
    deriving (Show,Eq)

prettyOneLine :: SExp -> String
prettyOneLine (SExp label children) =
    "(" ++ label ++ pChildren ++ ")"
    where
        pChildren = map (\c -> " " ++ prettyOneLine c)