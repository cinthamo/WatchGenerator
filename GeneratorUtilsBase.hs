module GeneratorUtilsBase where

import Model.GXModelBase
import Data.Char
import Data.List

fixControlName :: String -> String
fixControlName = let
                   repl '&' = '_'
                   repl  c   = c
                 in map repl

getHeight :: GXLayoutElementSpecific -> Maybe Double
getHeight table@GXLayoutElementTable{} = case height table of
                                           GXLayoutDimensionPoint n -> Just n
                                           _ -> Nothing
getHeight _ = Nothing

getSourceIndex :: [GXLayoutElement] -> [GXDataElement] -> Maybe Int
getSourceIndex (GXLayoutElement _ grid@GXLayoutElementGrid{}:_) ds = findIndex (\d -> dataProvider d == gridDataProvider grid) ds
getSourceIndex _ _ = Nothing

getDataElement :: String -> [GXDataElement] -> Maybe GXDataElement
getDataElement aDataProvider l@(_:_) =
  case filter (\x -> dataProvider x == aDataProvider) l of
    [] -> Nothing
    (y:_) -> Just y
getDataElement _ [] = Nothing

findUpdateProperty :: String -> Maybe GXDataElement -> Maybe String
findUpdateProperty _ Nothing = Nothing
findUpdateProperty st (Just (GXDataElement _ aVariables aAttributes)) =
             let st1 = map toLower $ filter (/= '_') st in
               case filterCompare variableName aVariables st1 of
                 (v:_) -> Just (variableName v)
                 [] -> case filterCompare attributeNameDrop aAttributes st1 of
                         (a:_) -> Just (attributeNameDrop a)
                         [] -> Nothing
             where
               filterCompare f xs st1 = filter (\x -> map toLower (f x) == st1) xs
               attributeNameDrop = drop 37 . attributeName -- 37 is the guid

emptyToNothing :: [a] -> Maybe [a]
emptyToNothing [] = Nothing
emptyToNothing xl = Just xl
