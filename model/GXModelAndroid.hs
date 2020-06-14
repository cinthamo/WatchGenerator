module Model.GXModelAndroid where

-- GXAppModel representa el modelo de un app GeneXus, con una colección de paneles
data GXAppModel = GXAppModel {
    mainPanelName :: String,
    panelObjects  :: [GXPanelObject]
  } deriving (Show, Eq)

-- GXPanelObject representa un panel GeneXus
data GXPanelObject = GXPanelObject {
       panelName         :: String,
       panelDataProvider :: String,
       dataList          :: [GXDataElement],
       layoutTable       :: GXLayoutElement -- Multiple Layout not supported
    } deriving (Show, Eq)

data GXDataElement = GXDataElement {
    dataProvider :: String,
    variables    :: [GXDataVariableElement],
    attributes   :: [GXDataAttributeElement]
  } deriving (Show, Eq)

data GXDataVariableElement = GXDataVariableElement {
    variableName :: String,
    variableType :: GXDataType
  } deriving (Show, Eq)

data GXDataAttributeElement = GXDataAttributeElement {
    attributeName :: String,
    attributeType :: GXDataType
  } deriving (Show, Eq)

data GXDataType = GXDataTypeCharacter
                | GXDataTypeNumeric
                | GXDataTypeImage
    deriving (Show, Eq)

data GXLayoutDimension = GXLayoutDimensionPercent Double | GXLayoutDimensionPoint Double
    deriving (Show, Eq)

data GXLayoutLabelPositionType = GXLayoutLabelPositionTypeNone
                               | GXLayoutLabelPositionTypeLeft
                               | GXLayoutLabelPositionTypeTop
                               | GXLayoutLabelPositionTypeRight
                               | GXLayoutLabelPositionTypeBottom
    deriving (Show, Eq)

-- GXLayoutElement representa los elementos (controles) que forman parte del layout de un panel GeneXus
data GXLayoutElement = GXLayoutElement GXLayoutElementBase GXLayoutElementSpecific
    deriving (Show, Eq)

newtype GXLayoutElementBase = GXLayoutElementBase {
       controlName   :: String
    } deriving (Show, Eq)

data GXLayoutElementSpecific = GXLayoutElementAction {
      actionName     :: String,
      caption        :: String
    }
                             | GXLayoutElementData {
      caption        :: String,
      labelPosition  :: GXLayoutLabelPositionType,
      readonly       :: Bool,
      controlType    :: String
    }
                             | GXLayoutElementTextBlock {
      caption        :: String
    }
                             | GXLayoutElementImage {
    }
                             | GXLayoutElementTable {
      rows               :: [[GXLayoutElement]],
      height             :: GXLayoutDimension
    }
                             | GXLayoutElementGrid {
      gridLayouts        :: [GXLayoutElement],
      gridDataProvider   :: String
    } deriving (Show, Eq)
