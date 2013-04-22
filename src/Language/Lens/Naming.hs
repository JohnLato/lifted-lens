module Language.Lens.Naming
    ( makeFieldName
    , unCamelCase
    ) where

import Data.Char (isUpper, toLower, isDigit)
import Data.List (intercalate, isPrefixOf)

makeFieldName :: String -> String -> String
makeFieldName dtn = validPrefix .
    unCamelCase . dropLowerPrefix dtn . dropLeadingUnderscore

dropLowerPrefix :: String -> String -> String
dropLowerPrefix prefix str
    | map toLower prefix `isPrefixOf` map toLower str = drop (length prefix) str
    | otherwise                                       = str

dropLeadingUnderscore :: String -> String
dropLeadingUnderscore ('_' : xs) = xs
dropLeadingUnderscore xs         = xs

unCamelCase :: String -> String
unCamelCase = intercalate "_" . map (map toLower) . caseGroup

validPrefix :: String -> String
validPrefix str
    | null str || isDigit (head str) = '_' : str
    | otherwise                      = str

-- | Group a name based on caps in a more or less intuitive way
--
-- > caseGroup "Person"      == ["Person"]
-- > caseGroup "IORef"       == ["IO", "Ref"]
-- > caseGroup "FooBar"      == ["Foo", "Bar"]
-- > caseGroup "RequestHTTP" == ["Request", "HTTP"]
-- > caseGroup "HTML5"       == ["HTML5"]
--
caseGroup :: String -> [String]
caseGroup = mergeSingles . caseGroup'
  where
    mergeSingles xs = case rest of
        (y : ys) -> merged ++ [y] ++ mergeSingles ys
        []       -> merged
      where
        (ss, rest) = break (not . isSingle) xs
        merged     = if null ss then [] else [concat ss]

    isSingle [_] = True
    isSingle _   = False

    caseGroup' []    = []
    caseGroup' (h : str)
        | null xs   = [h : x]
        | null x    = [h] : caseGroup' xs
        | otherwise = (h : x) : caseGroup' xs
      where
        (x, xs) = break (\c -> isUpper c || isDigit c) str
