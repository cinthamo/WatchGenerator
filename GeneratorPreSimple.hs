module GeneratorPreSimple (generatePreSimple) where

import Model.GXModelAndroid
import Templates.AndroidSimpleDataType
import GeneratorUtilsAndroid

generatePreSimple :: GXAppModel -> DataSimple
generatePreSimple model =
  let panel = head $ panelObjects model
      aPanelDataProvider = panelDataProvider panel
      d = getDataElement aPanelDataProvider aDataList
      GXLayoutElement _ table = layoutTable panel
      r = concat $ rows table
      aGridList = filter isGrid r
      aDataList = dataList panel
      aSourceIndex = getSourceIndex aGridList aDataList
      aDataView = Just (panelName panel ++ ".Level.Detail")
      aPackage = "com.artech.masterwatch." ++ mainPanelName model in
    DataSimple aPackage aDataView aSourceIndex $ mainLayout d r : map (gridLayout aDataList) aGridList
  where
    isGrid (GXLayoutElement _ GXLayoutElementGrid{}) = True
    isGrid _ = False

mainLayout :: Maybe GXDataElement -> [GXLayoutElement] -> LayoutSimple
mainLayout d l = LayoutSimple Main Vertical Nothing $ map (layoutItem d) l

layoutItem :: Maybe GXDataElement -> GXLayoutElement -> ItemSimple
layoutItem d (GXLayoutElement base specific) =
  let n = fixControlName $ controlName base
      p = findUpdateProperty n d
      spec = layoutItemSpec specific in
      ItemSimple (ItemSimpleBase n p) spec

layoutItemSpec :: GXLayoutElementSpecific -> ItemSimpleSpecific
layoutItemSpec (GXLayoutElementData _ _ _ "Image") = ImageSimple 100.0 60.0
layoutItemSpec (GXLayoutElementData aCaption GXLayoutLabelPositionTypeNone True _) = TextSimple aCaption Nothing
layoutItemSpec (GXLayoutElementData aCaption _ _ _) = EditSimple aCaption "number"
layoutItemSpec (GXLayoutElementAction aActionName aCaption) = ButtonSimple aCaption aActionName
layoutItemSpec (GXLayoutElementTextBlock aCaption) = TextSimple aCaption Nothing
layoutItemSpec GXLayoutElementImage{} = ImageSimple 0 0
layoutItemSpec GXLayoutElementGrid{} = GridSimple
layoutItemSpec _ = error "Element type not supported"

gridLayout :: [GXDataElement] -> GXLayoutElement -> LayoutSimple
gridLayout aDataList (GXLayoutElement _ grid@GXLayoutElementGrid{}) =
  let GXLayoutElement _ table = head $ gridLayouts grid
      aRows = rows table
      d = getDataElement (gridDataProvider grid) aDataList
      aItems = map (layoutItem d) (concat aRows)
      aHeight = getHeight table
      aIsVertical = if length aRows /= 1 || length (head aRows) == 1 then Vertical else Horizontal in
  LayoutSimple Line aIsVertical aHeight aItems
gridLayout _ _ = error "Show only be called with grids"
