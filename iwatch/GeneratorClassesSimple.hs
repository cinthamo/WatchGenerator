module IWatch.GeneratorClassesSimple (generateClassesSimple) where

import Templates.AndroidSimpleDataType
import IWatch.SwiftInterfaceControllerClass
import GeneratorUtils

generateClassesSimple :: DataSimple -> String -> [InterfaceControllerClass]
generateClassesSimple d =
  classesFromLayouts (layouts d)

classesFromLayouts :: [LayoutSimple] -> String -> [InterfaceControllerClass]
classesFromLayouts [] _ = []
classesFromLayouts (panelObject:panelObjects1) projName1 =
  let
    pClasses = classesFromLayout panelObject projName1
    classes = classesFromLayouts panelObjects1 projName1
  in (pClasses ++ classes)

classesFromLayout :: LayoutSimple -> String -> [InterfaceControllerClass]
classesFromLayout layout projName =
  let
    (ibOutlets1, ibActions1, gridRowControllers) = itemsConnectionsAndIBOutletFromLayoutElement (head (items layout)) projName
    controllerClass = InterfaceControllerClass (classNameFromLayout layout) ibOutlets1 ibActions1 gridRowControllers
  in [controllerClass]

classNameFromLayout :: LayoutSimple -> String
classNameFromLayout layout = "GXInterfaceController_" ++ layoutName layout

itemsConnectionsAndIBOutletFromLayoutElement :: ItemSimple -> String -> ([IBOutlet], [IBAction], [(String, RowControllerClass)])
itemsConnectionsAndIBOutletFromLayoutElement lElem@(ItemSimple _ (TextSimple _ _)) _ =
  let
    itemIBOutlet = layoutElementBaseItemConnectionAndOutlet lElem
  in ([itemIBOutlet], [], [])
itemsConnectionsAndIBOutletFromLayoutElement lElem@(ItemSimple base ButtonSimple{}) _ =
  let
    actionSelector1 = gxControlIBActionSelectorName (Templates.AndroidSimpleDataType.id base)
    ibAction = IBAction actionSelector1 (IBActionTypeGXControlAction (Templates.AndroidSimpleDataType.id base))
    itemIBOutlet = layoutElementBaseItemConnectionAndOutlet lElem
  in ([itemIBOutlet], [ibAction], [])
itemsConnectionsAndIBOutletFromLayoutElement lElem@(ItemSimple _ (EditSimple _ LabelPositionNone _)) _ =
  let
    itemIBOutlet = layoutElementBaseItemConnectionAndOutlet lElem
  in ([itemIBOutlet], [], [])
itemsConnectionsAndIBOutletFromLayoutElement lElem@(ItemSimple base EditSimple{}) _ =
  let
    labelItem = TextSimple "" Nothing
    labelOutletPropertyName = gxControlIBOutletLabelPropertyName (Templates.AndroidSimpleDataType.id base)
    labelIBOutlet = IBOutlet labelOutletPropertyName (ibOutletInterfaceObjectFromItem labelItem)
    editorOutletPropertyName = gxControlIBOutletEditorPropertyName (Templates.AndroidSimpleDataType.id base)
    editorIBOutlet = IBOutlet editorOutletPropertyName (ibOutletInterfaceObjectFromItem labelItem)
    itemIBOutlet = layoutElementBaseItemConnectionAndOutlet lElem
  in ([itemIBOutlet, labelIBOutlet, editorIBOutlet], [], [])
{--itemsConnectionsAndIBOutletFromLayoutElement lElem@(ItemSimple base (GridSimple gridTables)) projName =
  let
    (subItems, rowControllers) = foldl (\(is, rs) (i, r) -> (is ++ [i], rs ++ r)) ([], []) subElements
    itemIBOutlet = layoutElementBaseItemConnectionAndOutlet lElem item
  in ([itemIBOutlet], [], rowControllers)--}
itemsConnectionsAndIBOutletFromLayoutElement _ _ = ([], [], [])

layoutElementBaseItemConnectionAndOutlet :: ItemSimple -> IBOutlet
layoutElementBaseItemConnectionAndOutlet (ItemSimple lBase lSpecific) =
  let
    outletPropertyName1 = gxControlIBOutletPropertyName (Templates.AndroidSimpleDataType.id lBase)
    ibOutlet = IBOutlet outletPropertyName1 (ibOutletInterfaceObjectFromItem lSpecific)
  in ibOutlet

ibOutletInterfaceObjectFromItem :: ItemSimpleSpecific -> WKInterfaceObject
ibOutletInterfaceObjectFromItem TextSimple{} = WKInterfaceLabel
ibOutletInterfaceObjectFromItem ButtonSimple{} = WKInterfaceButton
ibOutletInterfaceObjectFromItem _ = error "ibOutletInterfaceObjectFromItem error"
