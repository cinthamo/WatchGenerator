module IWatch.GeneratorStoryboardSimple (generateStoryboardSimple) where

import Templates.AndroidSimpleDataType
import IWatch.WatchKitStoryboard
import GeneratorUtils
import Data.Char
import System.Random

generateStoryboardSimple :: DataSimple -> String -> IO Document
generateStoryboardSimple d projName = do
  (intialScene, initialViewController1) <- initialViewControllerScene projName
  scenes1 <- scenesFromSimple d projName
  return (Document dependencies1 (intialScene:scenes1) initialViewController1) where
    dependencies1 = [
      Deployment "watchOS",
      PlugIn "com.apple.InterfaceBuilder.IBCocoaTouchPlugin" 11524,
      PlugIn "com.apple.InterfaceBuilder.IBWatchKitPlugin" 11508 ]

initialViewControllerScene :: String -> IO (Scene, ObjectID)
initialViewControllerScene projName = do
  sceneId1 <- randomIO
  (sceneObject, initialViewController1) <- initialViewControllerSceneObject
  return (Scene sceneId1 "GX Initial Controller" [sceneObject] (0, 0), initialViewController1) where
    initialViewControllerSceneObject = do
      objId <- randomIO
      return (SceneObject (ControllerBase objId "" [] []) (Controller "WKInterfaceController" (projName ++ "_WatchKit_App") "target"), objId)

scenesFromSimple :: DataSimple -> String -> IO [Scene]
scenesFromSimple d =
  sceneFromLayouts (layouts d) (300, 0) where
    sceneFromLayouts [] _ _ = return []
    sceneFromLayouts (layout:layouts1) (x, y) projName1 = do
      pScene <- sceneFromLayout layout (x, y) projName1
      scenes1 <- sceneFromLayouts layouts1 (x + 300, y) projName1
      return (pScene:scenes1)

sceneFromLayout :: LayoutSimple -> Point -> String -> IO Scene
sceneFromLayout layout scenePoint projName = do
  sceneId1 <- randomIO
  sceneObject <- sceneObjectFromLayout layout projName
  return (Scene sceneId1 sceneDesc [sceneObject] scenePoint) where
    sceneDesc = map toLower (layoutName layout)

sceneObjectFromLayout :: LayoutSimple -> String -> IO SceneObject
sceneObjectFromLayout layout projName = do
  controllerBase <- controllerBaseFromLayout layout projName
  return (SceneObject controllerBase controllerSpecific) where
    controllerSpecific = Controller (classNameFromLayout layout) (projName ++ "_WatchKit_Extension") ""

controllerBaseFromLayout :: LayoutSimple -> String -> IO ControllerBase
controllerBaseFromLayout layout projName = do
  controllerId1 <- randomIO
  (items1, connections) <- itemsConnectionsFromLayoutElement (head (items layout)) projName controllerId1
  return (ControllerBase controllerId1 controllerIdentifier1 items1 connections) where
    controllerIdentifier1 = map toLower (layoutName layout) ++ ".level.detail"

classNameFromLayout :: LayoutSimple -> String
classNameFromLayout layout = "GXInterfaceController_" ++ layoutName layout

itemsConnectionsFromLayoutElement :: ItemSimple -> String -> ObjectID -> IO ([Item], [Connection])
itemsConnectionsFromLayoutElement lElem@(ItemSimple base (TextSimple caption1 _)) _ _controllerId = do
  itemId1 <- randomIO
  let
    (hAlign, vAlign) = alignmentFromLayoutItemBase base
    itemBase = ItemBase itemId1 hAlign vAlign [] []
    itemSpecific = LabelItem (if null caption1 then " " else caption1)
    item = Item itemBase itemSpecific
    in do
      itemOutletConn <- layoutElementBaseItemConnection lElem item
      return ([item], [itemOutletConn])
itemsConnectionsFromLayoutElement lElem@(ItemSimple base action@ButtonSimple{}) _ controllerId1 = do
  itemId1 <- randomIO
  actionConnId <- randomIO
  let
    actionSelector1 = gxControlIBActionSelectorName (Templates.AndroidSimpleDataType.id base)
    actionConn = Connection (ConnectionBase actionConnId controllerId1) (Action actionSelector1)
    (hAlign, vAlign) = alignmentFromLayoutItemBase base
    itemBase = ItemBase itemId1 hAlign vAlign [] [actionConn]
    itemSpecific = ButtonItem (if null (text action) then " " else text action)
    item = Item itemBase itemSpecific
    in do
      itemOutletConn <- layoutElementBaseItemConnection lElem item
      return ([item], [itemOutletConn])
itemsConnectionsFromLayoutElement lElem@(ItemSimple base (EditSimple _ LabelPositionNone _)) _ _controllerId = do
  itemId1 <- randomIO
  let
    (hAlign, vAlign) = alignmentFromLayoutItemBase base
    itemBase = ItemBase itemId1 hAlign vAlign [] []
    itemSpecific = LabelItem " "
    item = Item itemBase itemSpecific
    in do
      itemOutletConn <- layoutElementBaseItemConnection lElem item
      return ([item], [itemOutletConn])
itemsConnectionsFromLayoutElement lElem@(ItemSimple base (EditSimple caption1 labPos _)) _ _controllerId = do
      itemId1 <- randomIO
      itemLabelId <- randomIO
      itemEditorId <- randomIO
      labelConnId <- randomIO
      editorConnId <- randomIO
      let
        (hAlign, vAlign) = alignmentFromLayoutItemBase base
        itemLabel = Item (ItemBase itemLabelId hAlign vAlign [] []) (LabelItem caption1)
        itemEditor = Item (ItemBase itemEditorId hAlign vAlign [] []) (LabelItem " ")
        subItemsReverse = labPos == LabelPositionRight || labPos == LabelPositionBottom
        subItems = if subItemsReverse then [itemEditor, itemLabel] else [itemLabel, itemEditor]
        itemVerticalLayout = labPos == LabelPositionTop || labPos == LabelPositionBottom
        item = Item (ItemBase itemId1 hAlign vAlign subItems []) (GroupItem itemVerticalLayout)
        labelOutletPropertyName = gxControlIBOutletLabelPropertyName (Templates.AndroidSimpleDataType.id base)
        labelConn = Connection (ConnectionBase labelConnId itemLabelId) (Outlet labelOutletPropertyName)
        editorOutletPropertyName = gxControlIBOutletEditorPropertyName (Templates.AndroidSimpleDataType.id base)
        editorConn = Connection (ConnectionBase editorConnId itemEditorId) (Outlet editorOutletPropertyName)
        in do
          itemOutletConn <- layoutElementBaseItemConnection lElem item
          return ([item], [itemOutletConn, labelConn, editorConn])
{--itemsConnectionsFromLayoutElement lElem@(ItemSimple base (GridSimple {- -gridTables--})) projName controllerId1 = do
  itemId1 <- randomIO
  subElements <- mapM (\gridTable -> gridRowControllerItemAndClassFromLayoutElement gridTable (controlName base) projName controllerId1) gridTables
  let
    (subItems, rowControllers) = foldl (\(is, rs) (i, r) -> (is ++ [i], rs ++ r)) ([], []) subElements
    (hAlign, vAlign) = alignmentFromLayoutElementBase base
    itemBase = ItemBase itemId1 hAlign vAlign subItems []
    item = Item itemBase TableItem
    in do
      itemOutletConn <- layoutElementBaseItemConnectionAndOutlet lElem item
      return ([item], [itemOutletConn])--}
itemsConnectionsFromLayoutElement _ _ _ = return ([], [])

layoutElementBaseItemConnection :: ItemSimple -> Item -> IO Connection
layoutElementBaseItemConnection (ItemSimple lBase _lSpecific) (Item iBase _iSpecific) = do
  connId <- randomIO
  let
    outletPropertyName1 = gxControlIBOutletPropertyName (Templates.AndroidSimpleDataType.id lBase)
    connection = Connection (ConnectionBase connId (itemId iBase)) (Outlet outletPropertyName1)
    in return connection

alignmentFromLayoutItemBase :: t -> (HorizontalAlignment, VerticalAlignment)
alignmentFromLayoutItemBase _base = (HorizontalAlignmentDefault, VerticalAlignmentDefault)
