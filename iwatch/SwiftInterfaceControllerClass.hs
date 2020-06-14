module IWatch.SwiftInterfaceControllerClass where

{-|

EJEMPLOS DE ARCHIVOS .swift A PARTIR DE DONDE SE BASAN LAS REPRESENTACIONES HASKELL

import GXCoreUI

class GXInterfaceController_iOSMainPanelWatchOS: GXBaseSDPanelWKInterfaceController {

	@IBOutlet @objc var gxControl_maintable: WKInterfaceGroup!
	@IBOutlet @objc var gxControl_simpletext: WKInterfaceGroup!
	@IBOutlet @objc var gxControlLabel_simpletext: WKInterfaceLabel!
	@IBOutlet @objc var gxControlEditor_simpletext: WKInterfaceLabel!
	@IBOutlet @objc var gxControl_textblock1: WKInterfaceLabel!
	@IBOutlet @objc var gxControl_helloworld: WKInterfaceButton!

	@IBAction func gxControlActionSelector_Helloworld() {
		self.handleGXControlWithBoolValueActionSelector(for: "Helloworld", interfaceObjectIdentifier: gxControl_helloworld.interfaceProperty)
	}
}




import GXCoreUI

class GXInterfaceController_SDTGrid: GXBaseSDPanelWKInterfaceController {

	@IBOutlet @objc var gxControl_maintable: WKInterfaceGroup!
	@IBOutlet @objc var gxControl_grid1: WKInterfaceTable!


	override func table(_ table: WKInterfaceTable, didSelectRowAt rowIndex: Int) {
		switch table {
		case gxControl_grid1:
			self.handleGXControlGrid("Grid1", interfaceObjectIdentifier: gxControl_grid1.interfaceProperty, didSelectRowAt: rowIndex)
		default:
			super.table(table, didSelectRowAt: rowIndex)
		}
	}


	// MARK: Grid1 Row Controllers

	class GXRowController_Grid1_Layout1: GXControlGridBaseRowController {

		@IBOutlet @objc var gxControl_grid1table: WKInterfaceGroup!
		@IBOutlet @objc var gxControl_ctldescription: WKInterfaceLabel!

	}
}

-}

-- WKInterfaceObject representa los tipos que pueden tener los controles de UI en swift
data WKInterfaceObject = WKInterfaceGroup | WKInterfaceLabel | WKInterfaceButton | WKInterfaceImage | WKInterfaceTable deriving (Show, Eq)

-- IBActionType representa los tipos de metodos a generar asociados a eventos de controles
data IBActionType = IBActionTypeGXControlAction String | IBActionTypeGXControlActionWithBool String
  deriving (Show, Eq)

-- IBOutlet representa la asociacion de una propiedad con su respectivo elemento de UI en el Storyboard
data IBOutlet = IBOutlet {
      ibOutletPropertyName :: String,
      ibOutletInterfaceObject :: WKInterfaceObject
    } deriving (Show, Eq)

-- IBAction representa  la asociacion entre un metodo con un evento  de un elemento de UI en el Storyboard
data IBAction = IBAction {
      ibActionSelectorName :: String,
      ibActionType :: IBActionType
    } deriving (Show, Eq)

-- InterfaceControllerClass representa una classe swift correspondiente a un panel GeneXus
data InterfaceControllerClass = InterfaceControllerClass String [IBOutlet] [IBAction] [(String, RowControllerClass)]
  deriving (Show, Eq)

-- RowControllerClass representa una classe swift correspondiente a un tipo de layout de un control de tipo Grid en GeneXus
data RowControllerClass = RowControllerClass String [IBOutlet] [IBAction]
  deriving (Show, Eq)

-- SwiftControllerClass es una abstraccion de los componentes comunes de una clase swift
class SwiftControllerClass c where
  className :: c -> String
  ibOutlets :: c -> [IBOutlet]
  ibActions :: c -> [IBAction]

instance SwiftControllerClass InterfaceControllerClass where
  className (InterfaceControllerClass cn _ _ _) = cn
  ibOutlets (InterfaceControllerClass _ o _ _) = o
  ibActions (InterfaceControllerClass _ _ a _) = a

instance SwiftControllerClass RowControllerClass where
  className (RowControllerClass cn _ _) = cn
  ibOutlets (RowControllerClass _ o _) = o
  ibActions (RowControllerClass _ _ a) = a
