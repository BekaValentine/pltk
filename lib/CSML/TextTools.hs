module CSML.TextTools where

import Data.List (intercalate)

indent :: Int -> [String] -> [String]
indent 0 xs = xs
indent n xs = map (replicate n ' ' ++) xs

unlines' :: [String] -> String
unlines' = intercalate "\n"