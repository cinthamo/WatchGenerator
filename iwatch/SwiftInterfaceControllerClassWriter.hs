{-# LANGUAGE OverloadedStrings #-}

module IWatch.SwiftInterfaceControllerClassWriter (generateSwiftClassFile, gxControlIBOutletPropertyName, gxControlIBOutletLabelPropertyName, gxControlIBOutletEditorPropertyName, gxControlIBActionSelectorName, normalizedControlName, normalizedLowercaseControlName) where
import IWatch.SwiftInterfaceControllerClass
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified System.FilePath as FP
import GeneratorUtils

{-|
Parámetro 1 (InterfaceControllerClass): data type correspondiente a una clase Swift
Parámetro 2 (FilePath): un path al directorio de destino, donde a partir del nombre de la clase Swift se infiere el nombre del
 archivo .swift y se obtiene el path completo del archivo para el fuente .swift que se genera
-}
generateSwiftClassFile :: InterfaceControllerClass -> FilePath -> IO ()
generateSwiftClassFile icClass destDirPath = BS.writeFile filePath (generateSwiftClassString icClass) where
  filePath = FP.combine destDirPath (FP.addExtension (className icClass) "swift")

generateSwiftClassString :: InterfaceControllerClass -> BS.ByteString
generateSwiftClassString icClass@(InterfaceControllerClass _ _ _ gridRowControllers) =
  "import GXCoreUI\n\n" `BS.append`
  generateSwiftClassWithoutCloseBracketString icClass "GXBaseSDPanelWKInterfaceController" "" `BS.append`
  (if null gridRowControllers then ""
    else
      generateGridRowControllersDidSelectFunctionString gridRowControllers `BS.append`
      BS.concat (map generateGridRowControllerString gridRowControllers)) `BS.append`
  "}"

generateSwiftClassWithoutCloseBracketString :: SwiftControllerClass scc => scc -> BS.ByteString -> BS.ByteString -> BS.ByteString
generateSwiftClassWithoutCloseBracketString icClass superclassName indetStr =
  let innerIntentStr = indetStr `BS.append` "\t" in
    indetStr `BS.append` "class " `BS.append` BS.pack (className icClass) `BS.append` ": " `BS.append` superclassName `BS.append` " {\n" `BS.append`
    innerIntentStr `BS.append` "\n" `BS.append`
    BS.concat (map (generateIBOuteletString innerIntentStr) (ibOutlets icClass)) `BS.append`
    innerIntentStr `BS.append` "\n" `BS.append`
    BS.concat (map (generateIBActionString innerIntentStr) (ibActions icClass))

generateGridRowControllersDidSelectFunctionString :: [(String, RowControllerClass)] -> BS.ByteString
generateGridRowControllersDidSelectFunctionString gridRowControllers =
  "\t\n" `BS.append`
  "\toverride func table(_ table: WKInterfaceTable, didSelectRowAt rowIndex: Int) {\n" `BS.append`
  "\t\tswitch table {\n" `BS.append`
  BS.concat (map generateGridRowControllerDidSelectCase gridRowControllers) `BS.append`
  "\t\tdefault:\n" `BS.append`
  "\t\t\tsuper.table(table, didSelectRowAt: rowIndex)\n" `BS.append`
  "\t\t}\n" `BS.append`
  "\t}\n" `BS.append`
  "\t\n" where
    generateGridRowControllerDidSelectCase (gridControlName, _rowControllerClass) =
      let gxControlGrid = "gxControl_" `BS.append` BS.pack (normalizedLowercaseControlName gridControlName) in
        "\t\tcase " `BS.append` gxControlGrid `BS.append` ":\n" `BS.append`
        "\t\t\tself.handleGXControlGrid(\"" `BS.append` BS.pack gridControlName `BS.append` "\", interfaceObjectIdentifier: " `BS.append` gxControlGrid `BS.append` ".interfaceProperty, didSelectRowAt: rowIndex)\n"

generateGridRowControllerString :: (String, RowControllerClass) -> BS.ByteString
generateGridRowControllerString (gridControlName, rowControllerClass) =
  "\t\n" `BS.append`
  "\t// MARK: " `BS.append` BS.pack gridControlName `BS.append` " Row Controllers\n" `BS.append`
  "\t\n" `BS.append`
  generateSwiftClassWithoutCloseBracketString rowControllerClass "GXControlGridBaseRowController" "\t" `BS.append`
  "\t}\n"

generateIBOuteletString :: BS.ByteString -> IBOutlet -> BS.ByteString
generateIBOuteletString indetStr (IBOutlet pName interfaceObject) =
  indetStr `BS.append` "@IBOutlet @objc var " `BS.append` BS.pack pName `BS.append` ": " `BS.append` BS.pack (show interfaceObject) `BS.append` "!\n"

generateIBActionString :: BS.ByteString -> IBAction -> BS.ByteString
generateIBActionString indetStr (IBAction selectorName (IBActionTypeGXControlAction controlName)) =
  indetStr `BS.append` "@IBAction func " `BS.append` BS.pack selectorName `BS.append` "() {\n" `BS.append`
  indetStr `BS.append` "\tself.handleGXControlWithBoolValueActionSelector(for: \"" `BS.append` BS.pack controlName `BS.append` "\", interfaceObjectIdentifier: " `BS.append` BS.pack (gxControlIBOutletPropertyName controlName) `BS.append` ".interfaceProperty)\n" `BS.append`
  indetStr `BS.append` "}\n" `BS.append`
  indetStr `BS.append` "\n"
generateIBActionString indetStr (IBAction selectorName (IBActionTypeGXControlActionWithBool controlName)) =
  indetStr `BS.append` "@IBAction func " `BS.append` BS.pack selectorName `BS.append` "(_ value: Bool) {\n" `BS.append`
  indetStr `BS.append` "\tself.handleGXControlWithBoolValueActionSelector(for: \"" `BS.append` BS.pack controlName `BS.append` "\", interfaceObjectIdentifier: " `BS.append` BS.pack (gxControlIBOutletPropertyName controlName) `BS.append` ".interfaceProperty, value: value)\n" `BS.append`
  indetStr `BS.append` "}\n" `BS.append`
  indetStr `BS.append` "\n"
