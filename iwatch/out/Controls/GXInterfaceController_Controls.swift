import GXCoreUI

class GXInterfaceController_Controls: GXBaseSDPanelWKInterfaceController {
	
	@IBOutlet @objc var gxControl_maintable: WKInterfaceGroup!
	@IBOutlet @objc var gxControl_num: WKInterfaceGroup!
	@IBOutlet @objc var gxControlLabel_num: WKInterfaceLabel!
	@IBOutlet @objc var gxControlEditor_num: WKInterfaceLabel!
	@IBOutlet @objc var gxControl_click: WKInterfaceButton!
	
	@IBAction func gxControlActionSelector_Click() {
		self.handleGXControlWithBoolValueActionSelector(for: "Click", interfaceObjectIdentifier: gxControl_click.interfaceProperty)
	}
	
}