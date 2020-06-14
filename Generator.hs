module Generator (generate) where

import Model.GXModelBase
import Templates.AndroidLayoutDataType
import Templates.AndroidMainDataType
import GeneratorUtilsBase
import Data.Maybe

generate :: GXAppModel -> (LayoutList, AndroidMain, Maybe AndroidAdapter)
generate model = let panel = head $ panelObjects model
                     aPanelDataProvider = panelDataProvider panel
                     aDataList = dataList panel
                     aDataView = Just (panelName panel ++ ".Level.Detail")
                     d = getDataElement aPanelDataProvider aDataList
                     GXLayoutElement _ table = layoutTable panel
                     r = concat $ rows table
                     aItems = layoutItems aPackage d r
                     aGridList = filter isGrid r
                     aSourceIndex = getSourceIndex aGridList aDataList
                     aHasGrid = [] /= aGridList
                     aPackage = "com.artech.masterwatch." ++ mainPanelName model in
  (Layout "main" aHasGrid True True Nothing aItems : map (gridLayout aPackage aDataList) aGridList,
   AndroidMain aPackage aDataView aSourceIndex aHasGrid (getUpdates r d) (getEvents r),
   if aHasGrid then Just (AndroidAdapter aPackage (getUpdatesGrid aGridList aDataList)) else Nothing)
  where
    isGrid (GXLayoutElement _ GXLayoutElementGrid{}) = True
    isGrid _ = False
    getUpdatesGrid (GXLayoutElement _ grid@GXLayoutElementGrid{}:_) aDataList =
                             let GXLayoutElement _ table = head $ gridLayouts grid
                                 r = concat $ rows table
                                 d = getDataElement (gridDataProvider grid) aDataList in
                             fromJust $ getUpdates r d
    getUpdatesGrid _ _ = error "Should be a grid"

layoutItems :: String -> Maybe GXDataElement -> [GXLayoutElement] -> [Item]
layoutItems aPackage d = map f where
  f (GXLayoutElement base specific) = let name = fixControlName $ controlName base in
    Item (ItemBase name) (layoutItem aPackage specific name d)

layoutItem :: String -> GXLayoutElementSpecific -> String -> Maybe GXDataElement -> ItemSpecific
layoutItem _ (GXLayoutElementData _ _ _ _ "Image") _ _ = Image 100.0 60.0
layoutItem _ (GXLayoutElementData _ aCaption GXLayoutLabelPositionTypeNone True _) _ _ = Text aCaption Nothing
layoutItem _ (GXLayoutElementData _ aCaption aLabelPosition _ _) name d = Edit (getInputType name d) aCaption (getLabelOrientation aLabelPosition) (getLabelBefore aLabelPosition) (getLabelAfter aLabelPosition)
  where
    getLabelOrientation GXLayoutLabelPositionTypeLeft = "horizontal"
    getLabelOrientation GXLayoutLabelPositionTypeRight = "horizontal"
    getLabelOrientation _ = "vertical"
    getLabelBefore GXLayoutLabelPositionTypeLeft = True
    getLabelBefore GXLayoutLabelPositionTypeTop = True
    getLabelBefore _ = False
    getLabelAfter GXLayoutLabelPositionTypeRight = True
    getLabelAfter GXLayoutLabelPositionTypeBottom = True
    getLabelAfter _ = False
layoutItem _ (GXLayoutElementAction aActionName _ aCaption _) _ _ = Button aCaption ("onButton" ++ aActionName)
layoutItem _ (GXLayoutElementTextBlock aCaption) _ _ = Text aCaption Nothing
layoutItem _ GXLayoutElementImage{} _ _ = Image 0 0
layoutItem p GXLayoutElementGrid{} _ _ = Grid p
layoutItem _ _ _ _ = error "Element type not supported"

gridLayout :: String -> [GXDataElement] -> GXLayoutElement -> Layout
gridLayout aPackage aDataList (GXLayoutElement _ grid@GXLayoutElementGrid{}) =
  let GXLayoutElement _ table = head $ gridLayouts grid
      aRows = rows table
      d = getDataElement (gridDataProvider grid) aDataList
      aItems = layoutItems aPackage d (concat aRows)
      aHeight = getHeight table
      aIsVertical = length aRows /= 1 || length (head aRows) == 1 in
  Layout "line" False False aIsVertical aHeight aItems
gridLayout _ _ _ = error "Show only be called with grids"

getEvents :: [GXLayoutElement] -> Maybe [EventInfo]
getEvents l = emptyToNothing $ concatMap f l
              where
                f (GXLayoutElement _ (GXLayoutElementAction aActionName _ _ _)) = [EventInfo ("onButton" ++ aActionName) aActionName]
                f _ = []

getUpdates :: [GXLayoutElement] -> Maybe GXDataElement -> Maybe [UpdateItem]
getUpdates l d = emptyToNothing $ concatMap f l
               where
                 f (GXLayoutElement base dt@GXLayoutElementData{}) =
                    let n = fixControlName $ controlName base in
                        case findUpdateProperty n d of
                          Nothing -> []
                          Just p | controlType dt == "Image" -> [UpdateImage n p True]
                                 | readonly dt && labelPosition dt == GXLayoutLabelPositionTypeNone
                                   -> [UpdateText n p True]
                                 | otherwise -> [UpdateEdit n p True]
                 f (GXLayoutElement base GXLayoutElementGrid{}) = [UpdateGrid (controlName base)]
                 f _ = []
