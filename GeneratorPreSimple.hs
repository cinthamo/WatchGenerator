module GeneratorPreSimple (generatePreSimple) where

import Model.GXModelAndroid
import Templates.AndroidSimpleDataType
import GeneratorUtilsAndroid

-- Pasa de estructura de json a estructura intermedia
generatePreSimple :: GXAppModel -> DataSimple
generatePreSimple model =
  let panel = head $ panelObjects model
      aPanelName = panelName panel
      aPanelDataProvider = panelDataProvider panel
      d = getDataElement aPanelDataProvider aDataList
      GXLayoutElement _ table = layoutTable panel
      r = concat $ rows table
      aGridList = filter isGrid r
      aDataList = dataList panel
      aSourceIndex = getSourceIndex aGridList aDataList
      aDataView = Just (panelName panel ++ ".Level.Detail")
      aPackage = "com.artech.masterwatch." ++ mainPanelName model in
    DataSimple aPackage aDataView aSourceIndex $ mainLayout aPanelName d r : map (gridLayout aPanelName aDataList) aGridList
  where
    isGrid (GXLayoutElement _ GXLayoutElementGrid{}) = True
    isGrid _ = False

mainLayout :: String -> Maybe GXDataElement -> [GXLayoutElement] -> LayoutSimple
mainLayout aPanelName d l = LayoutSimple aPanelName Main Vertical Nothing $ map (layoutItem d) l

layoutItem :: Maybe GXDataElement -> GXLayoutElement -> ItemSimple
layoutItem d (GXLayoutElement base specific) =
  let n = fixControlName $ controlName base
      p = findUpdateProperty n d
      spec = layoutItemSpec specific n d in
      ItemSimple (ItemSimpleBase n p) spec

layoutItemSpec :: GXLayoutElementSpecific -> String -> Maybe GXDataElement -> ItemSimpleSpecific
layoutItemSpec GXLayoutElementData{controlType = "Image"} _ _ = ImageSimple 100.0 60.0
layoutItemSpec GXLayoutElementData{caption = aCaption, Model.GXModel.labelPosition = GXLayoutLabelPositionTypeNone, readonly = True} _ _ = TextSimple aCaption Nothing
layoutItemSpec GXLayoutElementData{caption = aCaption, Model.GXModel.labelPosition = aLabelPosition, isPassword = aIsPassword} name d = EditSimple aCaption (convertLabelPosition aLabelPosition) (getInputType name d aIsPassword)
  where
    convertLabelPosition GXLayoutLabelPositionTypeLeft = LabelPositionLeft
    convertLabelPosition GXLayoutLabelPositionTypeRight = LabelPositionRight
    convertLabelPosition GXLayoutLabelPositionTypeTop = LabelPositionTop
    convertLabelPosition GXLayoutLabelPositionTypeBottom = LabelPositionBottom
    convertLabelPosition GXLayoutLabelPositionTypeNone = LabelPositionNone
layoutItemSpec GXLayoutElementAction{Model.GXModel.actionName = aActionName, caption = aCaption} _ _ = ButtonSimple aCaption aActionName
layoutItemSpec GXLayoutElementTextBlock{caption = aCaption} _ _ = TextSimple aCaption Nothing
layoutItemSpec GXLayoutElementImage{} _ _ = ImageSimple 0 0
layoutItemSpec GXLayoutElementGrid{} _ _ = GridSimple
layoutItemSpec _ _ _ = error "Element type not supported"

gridLayout :: String -> [GXDataElement] -> GXLayoutElement -> LayoutSimple
gridLayout aPanelName aDataList (GXLayoutElement _ grid@GXLayoutElementGrid{}) =
  let GXLayoutElement _ table = head $ gridLayouts grid
      aRows = rows table
      d = getDataElement (gridDataProvider grid) aDataList
      aItems = map (layoutItem d) (concat aRows)
      aHeight = getHeight table
      aIsVertical = if length aRows /= 1 || length (head aRows) == 1 then Vertical else Horizontal in
  LayoutSimple aPanelName Line aIsVertical aHeight aItems
gridLayout _ _ _ = error "Should only be called with grids"
