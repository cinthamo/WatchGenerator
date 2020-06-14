module Model.GXModelConverterAndroid (convertAndroid) where

import Model.GXModelBase
import Model.GXModelAndroid

convertAndroid :: Model.GXModelBase.GXAppModel -> Model.GXModelAndroid.GXAppModel
convertAndroid (Model.GXModelBase.GXAppModel aMainPanelName aPanelObjects) =
  Model.GXModelAndroid.GXAppModel aMainPanelName (map convertPanel aPanelObjects)

convertPanel :: Model.GXModelBase.GXPanelObject -> Model.GXModelAndroid.GXPanelObject
convertPanel (Model.GXModelBase.GXPanelObject aPanelName aPanelDataProvider aDataList aLayoutTable) =
  Model.GXModelAndroid.GXPanelObject aPanelName aPanelDataProvider (map convertData aDataList) (convertElement aLayoutTable)

convertData :: Model.GXModelBase.GXDataElement -> Model.GXModelAndroid.GXDataElement
convertData (Model.GXModelBase.GXDataElement aDataProvider aVariables aAttributes) =
  Model.GXModelAndroid.GXDataElement aDataProvider (map convertVariable aVariables) (map convertAttribute aAttributes)

convertVariable :: Model.GXModelBase.GXDataVariableElement -> Model.GXModelAndroid.GXDataVariableElement
convertVariable (Model.GXModelBase.GXDataVariableElement aVariableName aVariableType) =
  Model.GXModelAndroid.GXDataVariableElement aVariableName (convertType aVariableType)

convertAttribute :: Model.GXModelBase.GXDataAttributeElement -> Model.GXModelAndroid.GXDataAttributeElement
convertAttribute (Model.GXModelBase.GXDataAttributeElement aAttributeName aAttributeType) =
  Model.GXModelAndroid.GXDataAttributeElement aAttributeName (convertType aAttributeType)

convertType :: Model.GXModelBase.GXDataType -> Model.GXModelAndroid.GXDataType
convertType Model.GXModelBase.GXDataTypeNumeric = Model.GXModelAndroid.GXDataTypeNumeric
convertType Model.GXModelBase.GXDataTypeCharacter = Model.GXModelAndroid.GXDataTypeCharacter
convertType Model.GXModelBase.GXDataTypeImage = Model.GXModelAndroid.GXDataTypeImage

convertElement :: Model.GXModelBase.GXLayoutElement -> Model.GXModelAndroid.GXLayoutElement
convertElement (Model.GXModelBase.GXLayoutElement aLayoutBase aLayoutSpecific) =
  Model.GXModelAndroid.GXLayoutElement (convertElementBase aLayoutBase) (convertElementSpecific aLayoutSpecific)

convertElementBase :: Model.GXModelBase.GXLayoutElementBase -> Model.GXModelAndroid.GXLayoutElementBase
convertElementBase (Model.GXModelBase.GXLayoutElementBase aControlName _ _ _) =
  Model.GXModelAndroid.GXLayoutElementBase aControlName

convertElementSpecific :: Model.GXModelBase.GXLayoutElementSpecific -> Model.GXModelAndroid.GXLayoutElementSpecific
convertElementSpecific (Model.GXModelBase.GXLayoutElementAction aActionName _ aCaption _) =
  Model.GXModelAndroid.GXLayoutElementAction aActionName aCaption
convertElementSpecific (Model.GXModelBase.GXLayoutElementData _ aCaption aLabelPosition aReadonly aControlType) =
  Model.GXModelAndroid.GXLayoutElementData aCaption (convertLabel aLabelPosition) aReadonly aControlType
convertElementSpecific (Model.GXModelBase.GXLayoutElementTextBlock aCaption) =
  Model.GXModelAndroid.GXLayoutElementTextBlock aCaption
convertElementSpecific (Model.GXModelBase.GXLayoutElementImage _) =
  Model.GXModelAndroid.GXLayoutElementImage
convertElementSpecific (Model.GXModelBase.GXLayoutElementTable aRows _ aHeight _) =
  Model.GXModelAndroid.GXLayoutElementTable (map (map convertElement) aRows) (convertDimension aHeight)
convertElementSpecific (Model.GXModelBase.GXLayoutElementGrid aGridLayouts aGridDataProvider _) =
  Model.GXModelAndroid.GXLayoutElementGrid (map convertElement aGridLayouts) aGridDataProvider

convertLabel :: Model.GXModelBase.GXLayoutLabelPositionType -> Model.GXModelAndroid.GXLayoutLabelPositionType
convertLabel Model.GXModelBase.GXLayoutLabelPositionTypeNone = Model.GXModelAndroid.GXLayoutLabelPositionTypeNone
convertLabel Model.GXModelBase.GXLayoutLabelPositionTypeLeft = Model.GXModelAndroid.GXLayoutLabelPositionTypeLeft
convertLabel Model.GXModelBase.GXLayoutLabelPositionTypeTop = Model.GXModelAndroid.GXLayoutLabelPositionTypeTop
convertLabel Model.GXModelBase.GXLayoutLabelPositionTypeRight = Model.GXModelAndroid.GXLayoutLabelPositionTypeRight
convertLabel Model.GXModelBase.GXLayoutLabelPositionTypeBottom = Model.GXModelAndroid.GXLayoutLabelPositionTypeBottom

convertDimension :: Model.GXModelBase.GXLayoutDimension -> Model.GXModelAndroid.GXLayoutDimension
convertDimension (Model.GXModelBase.GXLayoutDimensionPercent aDouble) =
  Model.GXModelAndroid.GXLayoutDimensionPercent aDouble
convertDimension (Model.GXModelBase.GXLayoutDimensionPoint aDouble) =
  Model.GXModelAndroid.GXLayoutDimensionPoint aDouble
