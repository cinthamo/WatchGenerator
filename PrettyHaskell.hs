module PrettyHaskell (ppMetadata, ppExample) where

import Model.GXModelBase
import Templates.AndroidLayoutDataType
import Templates.AndroidMainDataType
import Text.Show.Pretty (ppShow)
import Data.Char
import Data.Maybe
import Data.List.Utils

ppMetadata :: String -> GXAppModel -> String
ppMetadata name model = let (x:xs) = name; nameL = (toLower x:xs) in
  "module Metadata" ++ name ++ " where\n\n" ++
  "import Model.GXModelBase\n\n" ++
  nameL ++ " :: GXAppModel\n" ++
  nameL ++ " = " ++ ppShow model

ppExample :: String -> (LayoutList, AndroidMain, Maybe AndroidAdapter) -> String
ppExample name (layout, main, adapter) = let (x:xs) = name; nameL = (toLower x:xs) in
  "module Templates.Example" ++ name ++ " where\n\n" ++
  "import Templates.AndroidLayoutDataType\n" ++
  "import Templates.AndroidMainDataType\n\n" ++
  nameL ++ "Layout :: LayoutList\n" ++
  nameL ++ "Layout = " ++ replaceL ["id", "package"] "Templates.AndroidLayoutDataType" (ppShow layout) ++ "\n\n" ++
  nameL ++ "Main :: AndroidMain\n" ++
  nameL ++ "Main = " ++ replaceL ["id", "package"] "Templates.AndroidMainDataType" (ppShow main) ++
  if isJust adapter then
    "\n\n" ++
    nameL ++ "Adapter :: AndroidAdapter\n" ++
    nameL ++ "Adapter = " ++ replaceL ["id"] "Templates.AndroidMainDataType" (ppShow (fromJust adapter))
  else
    ""

replaceL :: [String] -> String -> String -> String
replaceL l p st = replace "\n" "\n  " $
    foldr (\a c -> replace ("{ " ++ a ++ " =") ("{ " ++ p ++ "." ++ a ++ " =") c) st l
