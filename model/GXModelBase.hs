module Model.GXModelBase where

data GXLayoutElementType = GXLayoutElementTypeTable
                         | GXLayoutElementTypeRow
                         | GXLayoutElementTypeCell
                         | GXLayoutElementTypeTabs
                         | GXLayoutElementTypeGroup
                         | GXLayoutElementTypeData
                         | GXLayoutElementTypeAction
                         | GXLayoutElementTypeTextBlock
                         | GXLayoutElementTypeImage
                         | GXLayoutElementTypeContent
                         | GXLayoutElementTypeGrid
                         | GXLayoutElementTypeUserControl
                         | GXLayoutElementTypeComponent
    deriving (Show, Eq)

-- GXAppModel representa el modelo de un app GeneXus, con una colección de paneles
data GXAppModel = GXAppModel {
    mainPanelName :: String,
    panelObjects  :: [GXPanelObject]
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

-- GXPanelObject representa un panel GeneXus
data GXPanelObject = GXPanelObject {
       panelName         :: String,
       panelDataProvider :: String,
       dataList          :: [GXDataElement],
       layoutTable       :: GXLayoutElement -- Multiple Layout not supported
    } deriving (Show, Eq)

data GXLayoutInvisibleMode = GXLayoutInvisibleModeKeepSpace | GXLayoutInvisibleModeCollapseSpace
    deriving (Show, Eq)

data GXHorizontalAlignType = GXHorizontalAlignTypeDefault
                           | GXHorizontalAlignTypeLeft
                           | GXHorizontalAlignTypeCenter
                           | GXHorizontalAlignTypeRight
    deriving (Show, Eq)

data GXVerticalAlignType = GXVerticalAlignTypeDefault
                         | GXVerticalAlignTypeTop
                         | GXVerticalAlignTypeCenter
                         | GXVerticalAlignTypeBottom
    deriving (Show, Eq)

type GXAlignType = (GXHorizontalAlignType, GXVerticalAlignType)

data GXLayoutDimension = GXLayoutDimensionPercent Double | GXLayoutDimensionPoint Double
    deriving (Show, Eq)

layoutDimensionHunderdPercent :: GXLayoutDimension
layoutDimensionHunderdPercent = GXLayoutDimensionPercent 100.0

data GXLayoutLabelPositionType = GXLayoutLabelPositionTypeNone
                               | GXLayoutLabelPositionTypeLeft
                               | GXLayoutLabelPositionTypeTop
                               | GXLayoutLabelPositionTypeRight
                               | GXLayoutLabelPositionTypeBottom
    deriving (Show, Eq)

-- GXLayoutElement representa los elementos (controles) que forman parte del layout de un panel GeneXus
data GXLayoutElement = GXLayoutElement GXLayoutElementBase GXLayoutElementSpecific
    deriving (Show, Eq)

data GXLayoutElementBase = GXLayoutElementBase {
       controlName   :: String,
       visible       :: Bool,
       enabled       :: Bool,
       invisibleMode :: GXLayoutInvisibleMode
    } deriving (Show, Eq)

data GXLayoutElementSpecific = GXLayoutElementAction {
      actionName     :: String,
      size           :: (Double, Double),
      caption        :: String,
      imageName      :: String
    }
                             | GXLayoutElementData {
      fieldName      :: String,
      caption        :: String,
      labelPosition  :: GXLayoutLabelPositionType,
      readonly       :: Bool,
      controlType    :: String
    }
                             | GXLayoutElementTextBlock {
      caption        :: String
    }
                             | GXLayoutElementImage {
      imageName      :: String
    }
                             | GXLayoutElementTable {
      rows               :: [[GXLayoutElement]],
      width              :: GXLayoutDimension,
      height             :: GXLayoutDimension,
      tableLayoutName    :: Maybe String
    }
                             | GXLayoutElementGrid {
      gridLayouts        :: [GXLayoutElement],
      gridDataProvider   :: String,
      gridDefaultAction  :: String
    } deriving (Show, Eq)
