module GeneratorUtils where

import Model.GXModel
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

findDataValueProperty :: String -> Maybe GXDataElement -> Maybe (String, GXDataType)
findDataValueProperty _ Nothing = Nothing
findDataValueProperty st (Just (GXDataElement _ aVariables aAttributes)) =
             let st1 = map toLower $ filter (/= '_') st in
               case filterCompare variableName aVariables st1 of
                 (v:_) -> Just (variableName v, variableType v)
                 [] -> case filterCompare attributeNameDrop aAttributes st1 of
                         (a:_) -> Just (attributeNameDrop a, attributeType a)
                         [] -> Nothing
             where
               filterCompare f xs st1 = filter (\x -> map toLower (f x) == st1) xs
               attributeNameDrop = drop 37 . attributeName -- 37 is the guid

findUpdateProperty :: String -> Maybe GXDataElement -> Maybe String
findUpdateProperty st d = case findDataValueProperty st d of
                                    Nothing -> Nothing
                                    Just (s, _) -> Just s

findDataTypeProperty :: String -> Maybe GXDataElement -> Maybe GXDataType
findDataTypeProperty st d = case findDataValueProperty st d of
                                    Nothing -> Nothing
                                    Just (_, dt) -> Just dt

getInputType :: String -> Maybe GXDataElement -> Bool -> String
getInputType _ _ True = "password"
getInputType name d _ = case findDataTypeProperty name d of
                          Just GXDataTypeNumeric -> "number"
                          _ -> "text"

emptyToNothing :: [a] -> Maybe [a]
emptyToNothing [] = Nothing
emptyToNothing xl = Just xl

{-- Apple --}

gxControlIBOutletPropertyName :: String -> String
gxControlIBOutletPropertyName controlName1 = "gxControl_" ++ normalizedLowercaseControlName controlName1

gxControlIBOutletLabelPropertyName :: String -> String
gxControlIBOutletLabelPropertyName controlName1 = "gxControlLabel_" ++ normalizedLowercaseControlName controlName1

gxControlIBOutletEditorPropertyName :: String -> String
gxControlIBOutletEditorPropertyName controlName1 = "gxControlEditor_" ++ normalizedLowercaseControlName controlName1

gxControlIBActionSelectorName :: String -> String
gxControlIBActionSelectorName controlName1 = "gxControlActionSelector_" ++ normalizedControlName controlName1

normalizedControlName :: String -> String
normalizedControlName controlName1
  | null controlName1 = controlName1
  | head controlName1 == '&' = tail controlName1
  | otherwise = controlName1

normalizedLowercaseControlName :: String -> String
normalizedLowercaseControlName controlName1 = map toLower (normalizedControlName controlName1)
