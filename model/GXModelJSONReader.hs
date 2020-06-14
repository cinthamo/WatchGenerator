{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model.GXModelJSONReader (parseApplicationModel) where

import Model.GXModelBase
import Data.Aeson ((.:), (.:?), (.!=), decode, FromJSON(..), Value(..), Object)
import Data.Aeson.Types (typeMismatch, fromJSON, Result(..), Parser, parseMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (stripSuffix, unpack, Text)
import Data.Char (toLower)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.Applicative ((<$>), (<*>), (<|>), empty)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified System.FilePath as FP

{-|
La implementación de la clase FromJSON, consiste en implementar una única función:
parseJSON :: Value -> Parser a
donde se devuelve un parser aplicativo a partir de un parámetro de tipo Value
(definido en el módulo Data.Aeson) que corresponde a una representación de tokens
para la sintaxis de JSON
-}

instance FromJSON GXLayoutDimension where
  parseJSON (Number n) = GXLayoutDimensionPoint <$> pure (toRealFloat n)
  parseJSON (String s) = (GXLayoutDimensionPercent <$>
                            case stripSuffix "%" s of
                              Just prefixS -> pure (read (unpack prefixS))
                              Nothing -> empty)
                          <|>
                          (GXLayoutDimensionPoint <$>
                            case stripSuffix "dip" s of
                              Just prefixS -> pure (read (unpack prefixS))
                              Nothing -> pure (read (unpack s)))
  parseJSON invalid = typeMismatch "GXLayoutDimension" invalid

instance FromJSON GXLayoutInvisibleMode where
  parseJSON (String "Keep Space") = pure GXLayoutInvisibleModeKeepSpace
  parseJSON (String "Collapse Space") = pure GXLayoutInvisibleModeCollapseSpace
  parseJSON invalid = typeMismatch "GXLayoutInvisibleMode" invalid

instance FromJSON GXLayoutLabelPositionType where
  parseJSON (String "Platform Default") = pure GXLayoutLabelPositionTypeTop
  parseJSON (String "Left") = pure GXLayoutLabelPositionTypeLeft
  parseJSON (String "None") = pure GXLayoutLabelPositionTypeNone
  parseJSON (String "Top") = pure GXLayoutLabelPositionTypeTop
  parseJSON (String "Right") = pure GXLayoutLabelPositionTypeRight
  parseJSON (String "Bottom") = pure GXLayoutLabelPositionTypeBottom
  parseJSON invalid = typeMismatch "GXLayoutLabelPositionType" invalid

instance FromJSON GXLayoutElementBase where
  parseJSON (Object v) = GXLayoutElementBase <$>
                         v .: "@controlName" <*>
                         parseMaybeBoolStringFromObject "@visible" v .!= True <*>
                         parseMaybeBoolStringFromObject "@enabled" v .!= True <*>
                         v .:? "@invisibleMode" .!= GXLayoutInvisibleModeKeepSpace
  parseJSON invalid = typeMismatch "GXLayoutElementBase" invalid

{-parseBoolString :: Value -> Parser Bool
parseBoolString (String "True") = return True
parseBoolString (String "False") = return False
parseBoolString (String _) = fail "Could not convert String to Bool"
parseBoolString invalid = typeMismatch "BoolString" invalid-}

parseMaybeBoolString :: Value -> Parser (Maybe Bool)
parseMaybeBoolString (String "True") = return (Just True)
parseMaybeBoolString (String "False") = return (Just False)
parseMaybeBoolString _ = return Nothing

parseMaybeBoolStringFromObject :: Text -> Object -> Parser (Maybe Bool)
parseMaybeBoolStringFromObject k v =
  case HM.lookup k v of
    Just boolValue -> parseMaybeBoolString boolValue
    Nothing -> return Nothing


instance FromJSON GXLayoutElementSpecific where
  parseJSON (Object v) = case HM.lookup "table" v of
                           Just (Object innerV) -> GXLayoutElementTable <$>
                                                     parseTableRows innerV <*>
                                                     innerV .:? "@width" .!= layoutDimensionHunderdPercent <*>
                                                     innerV .:? "@height" .!= layoutDimensionHunderdPercent <*>
                                                     innerV .:? "@layoutName"
                           _ -> empty
                         <|>
                         case HM.lookup "data" v of
                           Just (Object innerV) -> GXLayoutElementData <$>
                                                     innerV .: "@attribute" <*>
                                                     innerV .:? "@labelCaption" .!= "" <*>
                                                     innerV .:? "@labelPosition" .!= GXLayoutLabelPositionTypeTop <*>
                                                     parseMaybeBoolStringFromObject "@readonly" innerV .!= True <*>
                                                     case HM.lookup "CustomProperties" innerV of
                                                       Just (Object customV) -> customV .: "@ControlType"
                                                       _ -> pure ""
                                                     <*>
                                                     case HM.lookup "CustomProperties" innerV of
                                                       Just (Object customV) -> parseMaybeBoolStringFromObject "@IsPassword" customV .!= True
                                                       _ -> pure False
                           _ -> empty
                         <|>
                         case HM.lookup "textblock" v of
                           Just (Object innerV) -> GXLayoutElementTextBlock <$> innerV .:? "@caption" .!= ""
                           _ -> empty
                         <|>
                         case HM.lookup "action" v of
                           Just (Object innerV) -> GXLayoutElementAction <$>
                                                     innerV .: "@actionElement" <*>
                                                     pure (0, 0) <*>
                                                     innerV .:? "@actionElement" .!= "" <*>
                                                     pure ""
                           _ -> empty
                         <|>
                         case HM.lookup "grid" v of
                           Just (Object innerV) -> GXLayoutElementGrid <$>
                                                     parseGridLayouts innerV <*>
                                                     innerV .: "@DataProvider" <*>
                                                     innerV .:? "@defaultAction" .!= ""
                           _ -> empty
                         <|>
                         typeMismatch "GXLayoutElementSpecific" (Object v)

  parseJSON invalid = typeMismatch "GXLayoutElementSpecific" invalid

parseTableRows :: Object -> Parser [[GXLayoutElement]]
parseTableRows tableV = case HM.lookup "row" tableV of
                          Just (Array aRows) -> mapM parseTableRowCell (V.toList aRows)
                          Just obj@(Object _) -> do { row <- parseTableRowCell obj; return [row] }
                          Nothing -> pure []
                          Just invalid -> typeMismatch "GXLayoutElementRow" invalid

parseTableRowCell :: Value -> Parser [GXLayoutElement]
parseTableRowCell (Object row) = case HM.lookup "cell" row of
                                  Just (Array cells) -> mapM parseJSON (V.toList cells)
                                  Just cellV@(Object _) -> do { cell <- parseJSON cellV :: Parser GXLayoutElement; return [cell] } <|> return []
                                  Nothing -> pure []
                                  Just invalid -> typeMismatch "GXLayoutElementCell" invalid
parseTableRowCell _ = empty


parseGridLayouts :: Object -> Parser [GXLayoutElement]
parseGridLayouts grid = case HM.lookup "table" grid of
                          Just (Array tables) -> mapM (parseJSON . Object . HM.singleton "table") (V.toList tables)
                          Just (Object _) -> do { gridLayout <- parseJSON (Object grid) :: Parser GXLayoutElement; return [gridLayout] }
                          Nothing -> pure []
                          Just invalid -> typeMismatch "GXLayoutElementGrid" invalid

instance FromJSON GXLayoutElement where
  parseJSON obj@(Object v) = case HM.lookup "table" v of
                                Just innerObj@(Object _) -> GXLayoutElement <$> parseJSON innerObj <*> parseJSON obj
                                Just invalid -> typeMismatch "GXLayoutElementTable" invalid
                                Nothing -> empty
                             <|>
                             case HM.lookup "data" v of
                                 Just innerObj@(Object _) -> GXLayoutElement <$> parseJSON innerObj <*> parseJSON obj
                                 Just invalid -> typeMismatch "GXLayoutElementData" invalid
                                 Nothing -> empty
                             <|>
                             case HM.lookup "textblock" v of
                                 Just innerObj@(Object _) -> GXLayoutElement <$> parseJSON innerObj <*> parseJSON obj
                                 Just invalid -> typeMismatch "GXLayoutElementTextBlock" invalid
                                 Nothing -> empty
                             <|>
                              case HM.lookup "action" v of
                                 Just innerObj@(Object _) -> GXLayoutElement <$> parseJSON innerObj <*> parseJSON obj
                                 Just invalid -> typeMismatch "GXLayoutElementAction" invalid
                                 Nothing -> empty
                             <|>
                             case HM.lookup "grid" v of
                               Just innerObj@(Object _) -> GXLayoutElement <$> parseJSON innerObj <*> parseJSON obj
                               Just invalid -> typeMismatch "GXLayoutElementGrid" invalid
                               Nothing -> empty
                             <|>
                             typeMismatch "GXLayoutElement" obj
  parseJSON invalid = typeMismatch "GXLayoutElement" invalid

--parseData :: Object -> [GXDataElement]
--parseData _ = []
parseData :: Object -> Parser [GXDataElement]
parseData v = case HM.lookup "data" v of
                Just (Array ds) -> mapM parseJSON (V.toList ds)
                Just dV@(Object _) -> do { d <- parseJSON dV; return [d] }
                Nothing -> pure []
                Just invalid -> typeMismatch "GXDataElement" invalid


instance FromJSON GXDataElement where
  parseJSON obj@(Object v) = GXDataElement <$> v .: "@DataProvider" <*> parseDataVariable obj <*> parseDataAttribute obj
  parseJSON invalid = typeMismatch "GXDataElement" invalid

parseDataVariable :: Value -> Parser [GXDataVariableElement]
parseDataVariable (Object v) = case HM.lookup "variable" v of
                                  Just (Array vars) -> mapM parseJSON (V.toList vars)
                                  Just varV@(Object _) -> do { var <- parseJSON varV :: Parser GXDataVariableElement; return [var] } <|> return []
                                  Nothing -> pure []
                                  Just invalid -> typeMismatch "GXDataVariableElement" invalid
parseDataVariable _ = empty

instance FromJSON GXDataVariableElement where
  parseJSON (Object v) = GXDataVariableElement <$> v .: "@Name" <*> v .: "@internalType"
  parseJSON invalid = typeMismatch "GXDataVariableElement" invalid

parseDataAttribute :: Value -> Parser [GXDataAttributeElement]
parseDataAttribute (Object v) = case HM.lookup "attribute" v of
                                  Just (Array atts) -> mapM parseJSON (V.toList atts)
                                  Just attV@(Object _) -> do { att <- parseJSON attV :: Parser GXDataAttributeElement; return [att] } <|> return []
                                  Nothing -> pure []
                                  Just invalid -> typeMismatch "GXDataAttributeElement" invalid
parseDataAttribute _ = empty

instance FromJSON GXDataAttributeElement where
  parseJSON (Object v) = GXDataAttributeElement <$> v .: "@attribute" <*> v .: "@internalType"
  parseJSON invalid = typeMismatch "GXDataAttributeElement" invalid

instance FromJSON GXDataType where
  parseJSON (String "1") = pure GXDataTypeCharacter
  parseJSON (String "2") = pure GXDataTypeNumeric
  parseJSON (String "7") = pure GXDataTypeImage
  parseJSON invalid = typeMismatch "GXDataType" invalid

{-|
Parámetro 1 (String): nombre del panel principal de la aplicación como fue definido en GeneXus
Parámetro 2 (FilePath): path del directorio donde se encuentra los archivos JSON correspondientes a la metadata generada por GeneXus
Salida (IO GXAppModel): estructura GXAppModel definida en el módulo GXModel, bajo la mónada IO, a consecuencia de que tiene que leer el archivo de entrada
-}
parseApplicationModel :: String -> FilePath -> IO GXAppModel
parseApplicationModel aMainPanelName metadataDirPath = do
  panelObjectsVector <- parseApplicationPanels aMainPanelName metadataDirPath
  -- print (V.toList panelObjectsVector)
  return (GXAppModel aMainPanelName (V.toList panelObjectsVector))

parseApplicationPanels :: String -> FilePath -> IO (V.Vector GXPanelObject)
parseApplicationPanels aMainPanelName metadataDirPath = do
  panelNames <- parseApplicationPanelsNames aMainPanelName metadataDirPath
  if V.null panelNames
    then fail "No panels"
    else V.mapM (\aPanelName -> parsePanel (unpack aPanelName) metadataDirPath) panelNames

parseApplicationPanelsNames :: String -> FilePath -> IO (V.Vector Text)
parseApplicationPanelsNames aMainPanelName metadataDirPath = do
  fileContent <- BS.readFile (FP.combine metadataDirPath (map toLower aMainPanelName ++ "_gxapp"))
  case dec fileContent of
    Just panelNames -> return panelNames
    Nothing -> fail ("Could not parse panels names: " ++ aMainPanelName)
  where
    dec fileContent = do
      (Object json) <- decode fileContent :: Maybe Value
      (Array appsJson) <- HM.lookup "app" json
      (Object appJson) <- V.find findAppsJson appsJson
      (Array patternsJson) <- HM.lookup "Patterns" appJson
      return (V.map mapVector (V.filter filterVector patternsJson))
      where
        findAppsJson value = case value of
          (Object appJson) -> case HM.lookup "Patterns" appJson of
            Just (Array _) -> True
            _ -> False
          _ -> False
        filterVector patternJsonValue = case patternJsonValue of
          (Object patternJson) -> case HM.lookup "Type" patternJson of
            Just (String "d82625fd-5892-40b0-99c9-5c8559c197fc") -> case HM.lookup "Name" patternJson of
              Just (String _) -> True
              _ -> False
            _ -> False
          _ -> False
        mapVector (Object patternJson) = let Just (String pName) = HM.lookup "Name" patternJson in pName
        mapVector _ = error "Should be an Object"

parsePanel :: String -> FilePath -> IO GXPanelObject
parsePanel aPanelName metadataDirPath = do
  fileContent <- BS.readFile (FP.combine metadataDirPath (map toLower aPanelName ++ "_panel"))
  case dec fileContent of
    Just panelObject -> return panelObject
    Nothing -> fail ("Could not parse panel: " ++ aPanelName)
  where
    dec fileContent = do
      (Object json) <- decode fileContent :: Maybe Value
      (Object instanceJson) <- HM.lookup "instance" json
      (Object levelJson) <- HM.lookup "level" instanceJson
      (Object detailJson) <- HM.lookup "detail" levelJson
      layoutJson <- HM.lookup "layout" detailJson
      listDataElement <- parseMaybe parseData detailJson
      let aDataProvider = case HM.lookup "@DataProvider" detailJson of
                            Just (String s) -> unpack s
                            _ -> "" in
        case (fromJSON layoutJson :: Result GXLayoutElement) of
          Success tableLayoutElement -> return (GXPanelObject aPanelName aDataProvider listDataElement tableLayoutElement)
          Error s -> error s
