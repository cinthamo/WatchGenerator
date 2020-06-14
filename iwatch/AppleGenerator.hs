module IWatch.AppleGenerator (storyboardAndClassesFromGXModel) where
import Data.Char
import Model.GXModel
import IWatch.WatchKitStoryboard
import IWatch.SwiftInterfaceControllerClass
import IWatch.SwiftInterfaceControllerClassWriter
import System.Random

{-|
Parámetro 1 (GXAppModel): estructuras correspondientes al modelo de la aplicación (GXAppModel, salida de la etapa anterior)
Parámetro 2 (String): nombre del proyecto Xcode a ser generado
Salida (IO (Document, [InterfaceControllerClass])): tupla compuesta por la estructura correspondiente al storyboard
 (definida en el módulo WatchKitStoryboard, apéndice A3), y una lista de estructuras que representan las clases controladores
 swift (definida en el módulo SwiftInterfaceControllerClass, apéndice A5).
 El motivo de que la salida de esta función esté bajo la mónada IO, se debió a la necesidad de generar identificadores aleatorios
 (data type ObjectID, definido en el módulo WatchKitStoryboard). Para esto se definió una instancia de la clase Random sobre
 este data type, y luego la función randomIO del modulo System.Random
-}
storyboardAndClassesFromGXModel :: GXAppModel -> String -> IO (Document, [InterfaceControllerClass])
storyboardAndClassesFromGXModel appModel projName = do
  (scenes1, classes) <- scenesAndClassesFromGXModel appModel projName
  document <- documentIO scenes1
  return (document, classes) where
    documentIO scenes1 = do
      (intialScene, initialViewController1) <- initialViewControllerScene projName
      return (Document dependencies1 (intialScene:scenes1) initialViewController1)
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

scenesAndClassesFromGXModel :: GXAppModel -> String -> IO ([Scene], [InterfaceControllerClass])
scenesAndClassesFromGXModel appModel =
  sceneAndClassesFromPanelObjects (panelObjects appModel) (300, 0) where
    sceneAndClassesFromPanelObjects [] _ _ = return ([], [])
    sceneAndClassesFromPanelObjects (panelObject:panelObjects1) (x, y) projName1 = do
      (pScene, pClasses) <- sceneAndClassesFromPanelObject panelObject (x, y) projName1
      (scenes1, classes) <- sceneAndClassesFromPanelObjects panelObjects1 (x + 300, y) projName1
      return (pScene:scenes1, pClasses ++ classes)

sceneAndClassesFromPanelObject :: GXPanelObject -> Point -> String -> IO (Scene, [InterfaceControllerClass])
sceneAndClassesFromPanelObject panelObject scenePoint projName = do
  sceneId1 <- randomIO
  (sceneObject, controllerClasses) <- sceneObjectAndClassesFromPanelObject panelObject projName
  return (Scene sceneId1 sceneDesc [sceneObject] scenePoint, controllerClasses) where
    sceneDesc = map toLower (panelName panelObject)

sceneObjectAndClassesFromPanelObject :: GXPanelObject -> String -> IO (SceneObject, [InterfaceControllerClass])
sceneObjectAndClassesFromPanelObject panelObject projName = do
  (controllerBase, controllerClasses) <- controllerBaseAndClassesFromPanelObject panelObject projName
  return (SceneObject controllerBase controllerSpecific, controllerClasses) where
    controllerSpecific = Controller (classNameFromPanelObject panelObject) (projName ++ "_WatchKit_Extension") ""

controllerBaseAndClassesFromPanelObject :: GXPanelObject -> String -> IO (ControllerBase, [InterfaceControllerClass])
controllerBaseAndClassesFromPanelObject panelObject projName = do
  controllerId1 <- randomIO
  (items, connections, ibOutlets1, ibActions1, gridRowControllers) <- itemsConnectionsAndIBOutletFromLayoutElement (layoutTable panelObject) projName controllerId1
  let
    controllerIdentifier1 = map toLower (panelName panelObject) ++ ".level.detail"
    controllerBase = ControllerBase controllerId1 controllerIdentifier1 items connections
    controllerClass = InterfaceControllerClass (classNameFromPanelObject panelObject) ibOutlets1 ibActions1 gridRowControllers
    in return (controllerBase, [controllerClass])

classNameFromPanelObject :: GXPanelObject -> String
classNameFromPanelObject panelObject = "GXInterfaceController_" ++ panelName panelObject

itemsConnectionsAndIBOutletFromLayoutElement :: GXLayoutElement -> String -> ObjectID -> IO ([Item], [Connection], [IBOutlet], [IBAction], [(String, RowControllerClass)])
itemsConnectionsAndIBOutletFromLayoutElement lElem@(GXLayoutElement base (GXLayoutElementTable rows1 _w _h _)) projName controllerId1 = do
  itemId1 <- randomIO
  subElements <- mapM (\row -> itemsConnectionsAndIBOutletFromLayoutElementTableRow row projName controllerId1) rows1
  let
    (subItems, subConnections, subIBOutlets, subIBActions, gridRowControllers) = foldl (\(is, ss, os, as, gs) (i, s, o, a, g) -> (is ++ i, ss ++ s, os ++ o, as ++ a, gs ++ g)) ([], [], [], [], []) subElements
    (hAlign, vAlign) = alignmentFromLayoutElementBase base
    itemBase = ItemBase itemId1 hAlign vAlign subItems []
    itemSpecific = GroupItem True
    item = Item itemBase itemSpecific
    in do
      (itemOutletConn, itemIBOutlet) <- layoutElementBaseItemConnectionAndOutlet lElem item
      return ([item], itemOutletConn : subConnections, itemIBOutlet : subIBOutlets, subIBActions, gridRowControllers)
itemsConnectionsAndIBOutletFromLayoutElement lElem@(GXLayoutElement base (GXLayoutElementTextBlock caption1)) _ _controllerId = do
  itemId1 <- randomIO
  let
    (hAlign, vAlign) = alignmentFromLayoutElementBase base
    itemBase = ItemBase itemId1 hAlign vAlign [] []
    itemSpecific = LabelItem (if null caption1 then " " else caption1)
    item = Item itemBase itemSpecific
    in do
      (itemOutletConn, itemIBOutlet) <- layoutElementBaseItemConnectionAndOutlet lElem item
      return ([item], [itemOutletConn], [itemIBOutlet], [], [])
itemsConnectionsAndIBOutletFromLayoutElement lElem@(GXLayoutElement base action@GXLayoutElementAction{}) _ controllerId1 = do
  itemId1 <- randomIO
  actionConnId <- randomIO
  let
    actionSelector1 = gxControlIBActionSelectorName (controlName base)
    actionConn = Connection (ConnectionBase actionConnId controllerId1) (Action actionSelector1)
    ibAction = IBAction actionSelector1 (IBActionTypeGXControlAction (controlName base))
    (hAlign, vAlign) = alignmentFromLayoutElementBase base
    itemBase = ItemBase itemId1 hAlign vAlign [] [actionConn]
    itemSpecific = ButtonItem (if null (caption action) then " " else caption action)
    item = Item itemBase itemSpecific
    in do
      (itemOutletConn, itemIBOutlet) <- layoutElementBaseItemConnectionAndOutlet lElem item
      return ([item], [itemOutletConn], [itemIBOutlet], [ibAction], [])
itemsConnectionsAndIBOutletFromLayoutElement lElem@(GXLayoutElement base (GXLayoutElementData _ _ GXLayoutLabelPositionTypeNone _ _ _)) _ _controllerId = do
  itemId1 <- randomIO
  let
    (hAlign, vAlign) = alignmentFromLayoutElementBase base
    itemBase = ItemBase itemId1 hAlign vAlign [] []
    itemSpecific = LabelItem " "
    item = Item itemBase itemSpecific
    in do
      (itemOutletConn, itemIBOutlet) <- layoutElementBaseItemConnectionAndOutlet lElem item
      return ([item], [itemOutletConn], [itemIBOutlet], [], [])
itemsConnectionsAndIBOutletFromLayoutElement lElem@(GXLayoutElement base (GXLayoutElementData _ caption1 labPos _ _ _)) _ _controllerId = do
      itemId1 <- randomIO
      itemLabelId <- randomIO
      itemEditorId <- randomIO
      labelConnId <- randomIO
      editorConnId <- randomIO
      let
        (hAlign, vAlign) = alignmentFromLayoutElementBase base
        itemLabel = Item (ItemBase itemLabelId hAlign vAlign [] []) (LabelItem caption1)
        itemEditor = Item (ItemBase itemEditorId hAlign vAlign [] []) (LabelItem " ")
        subItemsReverse = labPos == GXLayoutLabelPositionTypeRight || labPos == GXLayoutLabelPositionTypeBottom
        subItems = if subItemsReverse then [itemEditor, itemLabel] else [itemLabel, itemEditor]
        itemVerticalLayout = labPos == GXLayoutLabelPositionTypeTop || labPos == GXLayoutLabelPositionTypeBottom
        item = Item (ItemBase itemId1 hAlign vAlign subItems []) (GroupItem itemVerticalLayout)
        labelOutletPropertyName = gxControlIBOutletLabelPropertyName (controlName base)
        labelConn = Connection (ConnectionBase labelConnId itemLabelId) (Outlet labelOutletPropertyName)
        labelIBOutlet = IBOutlet labelOutletPropertyName (ibOutletInterfaceObjectFromItem itemLabel)
        editorOutletPropertyName = gxControlIBOutletEditorPropertyName (controlName base)
        editorConn = Connection (ConnectionBase editorConnId itemEditorId) (Outlet editorOutletPropertyName)
        editorIBOutlet = IBOutlet editorOutletPropertyName (ibOutletInterfaceObjectFromItem itemEditor)
        in do
          (itemOutletConn, itemIBOutlet) <- layoutElementBaseItemConnectionAndOutlet lElem item
          return ([item], [itemOutletConn, labelConn, editorConn], [itemIBOutlet, labelIBOutlet, editorIBOutlet], [], [])
itemsConnectionsAndIBOutletFromLayoutElement lElem@(GXLayoutElement base (GXLayoutElementGrid gridTables _ _defaultAction)) projName controllerId1 = do
  itemId1 <- randomIO
  subElements <- mapM (\gridTable -> gridRowControllerItemAndClassFromLayoutElement gridTable (controlName base) projName controllerId1) gridTables
  let
    (subItems, rowControllers) = foldl (\(is, rs) (i, r) -> (is ++ [i], rs ++ r)) ([], []) subElements
    (hAlign, vAlign) = alignmentFromLayoutElementBase base
    itemBase = ItemBase itemId1 hAlign vAlign subItems []
    item = Item itemBase TableItem
    in do
      (itemOutletConn, itemIBOutlet) <- layoutElementBaseItemConnectionAndOutlet lElem item
      return ([item], [itemOutletConn], [itemIBOutlet], [], rowControllers)
itemsConnectionsAndIBOutletFromLayoutElement _ _ _ = return ([], [], [], [], [])

itemsConnectionsAndIBOutletFromLayoutElementTableRow :: [GXLayoutElement] -> String -> ObjectID -> IO ([Item], [Connection], [IBOutlet], [IBAction], [(String, RowControllerClass)])
itemsConnectionsAndIBOutletFromLayoutElementTableRow [] _ _ = return ([], [], [], [], [])
itemsConnectionsAndIBOutletFromLayoutElementTableRow [cell] projName controllerId1 =
  itemsConnectionsAndIBOutletFromLayoutElement cell projName controllerId1
itemsConnectionsAndIBOutletFromLayoutElementTableRow cells projName controllerId1 = do
  itemId1 <- randomIO
  subElements <- mapM (\cell -> itemsConnectionsAndIBOutletFromLayoutElement cell projName controllerId1) cells
  let
    (subItems, subConnections, subIBOutlets, subIBActions, gridRowControllers) = foldl (\(is, ss, os, as, gs) (i, s, o, a, g) -> (is ++ i, ss ++ s, os ++ o, as ++ a, gs ++ g)) ([], [], [], [], []) subElements
    (hAlign, vAlign) = alignmentFromLayoutElementBase base
    base = ItemBase itemId1 hAlign vAlign subItems []
    specific = GroupItem False
    rowItem = Item base specific
    in return ([rowItem], subConnections, subIBOutlets, subIBActions, gridRowControllers)

gridRowControllerItemAndClassFromLayoutElement :: GXLayoutElement -> String -> String -> ObjectID -> IO (Item, [(String, RowControllerClass)])
gridRowControllerItemAndClassFromLayoutElement lElem@(GXLayoutElement _ (GXLayoutElementTable _ _ _ (Just layoutName))) gridName projName controllerId1 =
  do
    itemId1 <- randomIO
    ([rootItem], subConnections, subIBOutlets, subIBActions, gridRowControllers) <- itemsConnectionsAndIBOutletFromLayoutElement lElem projName controllerId1
    let
      rowControllerClassName = ("GXRowController_" ++ gridName ++ "_" ++ layoutName)
      (hAlign, vAlign) = alignmentFromLayoutElementBase base
      base = ItemBase itemId1 hAlign vAlign [] subConnections
      specific = TableRowItem layoutName True rowControllerClassName (projName ++ "_WatchKit_Extension") "" rootItem
      tableRowItem = Item base specific
      rowController = RowControllerClass rowControllerClassName subIBOutlets subIBActions
      in return (tableRowItem, (gridName, rowController) : gridRowControllers)
gridRowControllerItemAndClassFromLayoutElement _ _ _ _ = error "Invalid Grid Layout Element"

layoutElementBaseItemConnectionAndOutlet :: GXLayoutElement -> Item -> IO (Connection, IBOutlet)
layoutElementBaseItemConnectionAndOutlet (GXLayoutElement lBase _lSpecific) item@(Item iBase _iSpecific) = do
  connId <- randomIO
  let
    outletPropertyName1 = gxControlIBOutletPropertyName (controlName lBase)
    connection = Connection (ConnectionBase connId (itemId iBase)) (Outlet outletPropertyName1)
    ibOutlet = IBOutlet outletPropertyName1 (ibOutletInterfaceObjectFromItem item)
    in return (connection, ibOutlet)

ibOutletInterfaceObjectFromItem :: Item -> WKInterfaceObject
ibOutletInterfaceObjectFromItem (Item _ (GroupItem _)) = WKInterfaceGroup
ibOutletInterfaceObjectFromItem (Item _ (LabelItem _)) = WKInterfaceLabel
ibOutletInterfaceObjectFromItem (Item _ (ButtonItem _)) = WKInterfaceButton
ibOutletInterfaceObjectFromItem (Item _ TableItem) = WKInterfaceTable
ibOutletInterfaceObjectFromItem _ = error "ibOutletInterfaceObjectFromItem error"

alignmentFromLayoutElementBase :: t -> (HorizontalAlignment, VerticalAlignment)
alignmentFromLayoutElementBase _base = (HorizontalAlignmentDefault, VerticalAlignmentDefault)
