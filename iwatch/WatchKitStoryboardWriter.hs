 {-# LANGUAGE OverloadedStrings #-}

module IWatch.WatchKitStoryboardWriter (generateStoryboardFile) where
import IWatch.WatchKitStoryboard as W
import Text.XML.Writer as X
import qualified Text.XML as XML
import Control.Monad.Trans.Writer.Strict
import qualified Data.DList as D
import Data.Functor.Identity
import Data.String

{-|
Parámetro 1 (FilePath): path al archivo destino
Parámetro 2 (Document): estructura correspondiente al storyboard (definida en el módulo WatchKitStoryboard)
-}
generateStoryboardFile :: FilePath -> Document -> IO ()
generateStoryboardFile file wkDoc =
  -- X.pprint xmlDoc
  XML.writeFile XML.def file xmlDoc where
    xmlDoc = let [XML.NodeElement xmlDocElement] = render (toXML wkDoc) in XML.Document (XML.Prologue [] Nothing []) xmlDocElement []

foldToXML :: ToXML a => [a] -> Control.Monad.Trans.Writer.Strict.WriterT (D.DList XML.Node) Data.Functor.Identity.Identity ()
foldToXML [] = empty
foldToXML (x:xs) = do { toXML x; foldToXML xs }

instance ToXML W.Document where
  toXML (W.Document ds ss objID) =
      elementA "document" attributes $ do
      element "dependencies" $ foldToXML ds
      element "scenes" $ foldToXML ss
    where attributes = [
            ("type", "com.apple.InterfaceBuilder.WatchKit.Storyboard"),
            ("version", "3.0"),
            ("toolsVersion", "11542"),
            ("systemVersion", "16B2555"),
            ("targetRuntime", "watchKit"),
            ("propertyAccessControl", "none"),
            ("useAutolayout", "YES"),
            ("useTraitCollections", "YES"),
            ("colorMatched", "YES"),
            ("initialViewController", genObjectIDText objID) ]

genObjectIDString :: ObjectID -> String
genObjectIDString (ObjectID c1 c2 c3 c4 c5 c6 c7 c8) = [ c1, c2, c3, '-', c4, c5, '-', c6, c7, c8]

genObjectIDText :: IsString a => ObjectID -> a
genObjectIDText objID = fromString (genObjectIDString objID)

instance ToXML Dependency where
  toXML (PlugIn plugInIdentifier1 version1) =
    elementA "plugIn" [("identifier", fromString plugInIdentifier1), ("version", fromString (show version1))] empty
  toXML (Deployment deploymentIdentifier1) =
    elementA "deployment" [("identifier", fromString deploymentIdentifier1)] empty

instance ToXML Scene where
  toXML (Scene sceneId1 sceneDescription1 sceneObjects1 (x, y)) =
                      do
                        comment (fromString sceneDescription1)
                        elementA "scene" [("sceneID", genObjectIDText sceneId1)] $ do
                          element "objects" (foldToXML sceneObjects1)
                          elementA "point" [("key", "canvasLocation"), ("x", fromString (show x)), ("y", fromString (show y))] empty

instance ToXML SceneObject where
  toXML sObj@(SceneObject base (NotificationController cat)) =
    sceneObjectElemntA sObj $ do
      toXML base
      let (NotificationCategory objId identifier) = cat in
        elementA "notificationCategory" [("key", "notificationCategory"), ("identifier", fromString identifier), ("id", genObjectIDText objId)] empty
  toXML sObj@(SceneObject base _) = sceneObjectElemntA sObj $ toXML base

instance ToXML ControllerBase where
  toXML (ControllerBase _ _ controllerItems1 controllerConnections1) = do
            if null controllerItems1 then empty else element "items" (foldToXML controllerItems1)
            if null controllerConnections1 then empty else element "connections" (foldToXML controllerConnections1)

sceneObjectElemntA :: ToXML a => SceneObject -> a -> XML
sceneObjectElemntA (SceneObject base (Controller c m mp)) = let
  idAtt = [("id", genObjectIDText (controllerId base))]
  in elementA "controller" (idAtt ++ classModuleProvider c m mp)
sceneObjectElemntA (SceneObject base (NotificationController _)) =
  elementA "notificationController" [("id", genObjectIDText (controllerId base))]

classModuleProvider :: (IsString t1, IsString t) => String -> String -> String -> [(t1, t)]
classModuleProvider c m mp = let
  customClassAtt = if null c then [] else [("customClass", fromString c)]
  customModuleAtt = if null m then [] else [("customModule", fromString m)]
  customModuleProviderAtt = if null mp then [] else [("customModuleProvider", fromString mp)]
  in customClassAtt ++ customModuleAtt ++ customModuleProviderAtt

instance ToXML Item where
  toXML (Item base specific) = let
                itemAtts = itemBaseAttributes base ++ itemSpecificAttributes specific
                in elementA (itemSpecificNodeName specific) itemAtts $ do
                  itemSpecificNodes specific
                  itemBaseNodes base

itemBaseAttributes :: (IsString t, IsString t1) => ItemBase -> [(t1, t)]
itemBaseAttributes (ItemBase iId hAlign vAlign _ _) = let
          hAlignAtt = case hAlign of
                  HorizontalAlignmentDefault -> []
                  HorizontalAlignmentLeft -> [("alignment", "left")]
                  HorizontalAlignmentCenter -> [("alignment", "center")]
                  HorizontalAlignmentRight -> [("alignment", "right")]
          vAlignAtt = case vAlign of
                  VerticalAlignmentDefault -> []
                  VerticalAlignmentTop -> [("verticalAlignment", "top")]
                  VerticalAlignmentCenter -> [("verticalAlignment", "center")]
                  VerticalAlignmentBottom -> [("verticalAlignment", "bottom")]
          in ("id", genObjectIDText iId) : (("width", "1") : (hAlignAtt ++ vAlignAtt))

itemSpecificAttributes :: (IsString t, IsString t1) => ItemSpecific -> [(t1, t)]
itemSpecificAttributes (GroupItem vLayout) = [("layout", fromString "vertical") | vLayout]
itemSpecificAttributes (LabelItem text) = [("text", fromString text)]
itemSpecificAttributes (ButtonItem title) = [("title", fromString title)]
itemSpecificAttributes (TableRowItem identifier sel c m mp _) =
  let
    atts = [("identifier", fromString identifier)]
    selAtt = if sel then [] else [("selectable", fromString "NO")]
    in atts ++ selAtt ++ classModuleProvider c m mp
itemSpecificAttributes _ = []

itemSpecificNodeName :: IsString t => ItemSpecific -> t
itemSpecificNodeName (GroupItem _) = "group"
itemSpecificNodeName (LabelItem _) = "label"
itemSpecificNodeName (ButtonItem _) = "button"
itemSpecificNodeName TableItem = "table"
itemSpecificNodeName TableRowItem{} = "tableRow"

itemBaseNodes :: ItemBase -> WriterT (D.DList XML.Node) Identity ()
itemBaseNodes (ItemBase _ _ _ subitems connections) = do
                  if null subitems then empty else element "items" (foldToXML subitems)
                  if null connections then empty else element "connections" (foldToXML connections)

itemSpecificNodes :: ItemSpecific -> XML
itemSpecificNodes (TableRowItem _ _ _ _ _ rootItem) = toXML rootItem
itemSpecificNodes _ = empty

instance ToXML Connection where
  toXML (Connection base (Outlet pName)) =
    let { attributes = ("property", fromString pName) : connectionBaseXMLAttributes base }
    in elementA "outlet" attributes empty
  toXML (Connection base (Segue sKind sRel)) =
    let { attributes = connectionBaseXMLAttributes base ++ [ ("kind", fromString sKind), ("relationship", fromString sRel) ] }
    in elementA "segue" attributes empty
  toXML (Connection base (Action actionSel)) =
    let { attributes = ("selector", fromString actionSel) : connectionBaseXMLAttributes base }
    in elementA "action" attributes empty

connectionBaseXMLAttributes :: (IsString t, IsString t1) => ConnectionBase -> [(t1, t)]
connectionBaseXMLAttributes (ConnectionBase connectionId1 connectionDestination1) = [
  ("destination", genObjectIDText connectionDestination1),
  ("id", genObjectIDText connectionId1) ]
