import GXCoreUI

class GXInterfaceController_CountryList: GXBaseSDPanelWKInterfaceController {
	
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
		@IBOutlet @objc var gxControl_countryflag: WKInterfaceLabel!
		@IBOutlet @objc var gxControl_countryname: WKInterfaceLabel!
		
	}
}