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
                     aItems = layoutItems aPackage r
                     aGridList = filter isGrid r
                     aSourceIndex = getSourceIndex aGridList aDataList
                     aHasGrid = [] /= aGridList
                     aPackage = "com.artech.masterwatch." ++ mainPanelName model in
  (Layout "main" aHasGrid True True Nothing aItems : map (gridLayout aPackage) aGridList,
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

layoutItems :: String -> [GXLayoutElement] -> [Item]
layoutItems aPackage = map f where
  f (GXLayoutElement base specific) = Item (ItemBase (fixControlName $ controlName base)) (layoutItem aPackage specific)

layoutItem :: String -> GXLayoutElementSpecific -> ItemSpecific
layoutItem _ (GXLayoutElementData _ _ _ _ "Image") = Image 100.0 60.0
layoutItem _ (GXLayoutElementData _ aCaption GXLayoutLabelPositionTypeNone True _) = Text aCaption Nothing
layoutItem _ (GXLayoutElementData _ aCaption _ _ _) = Edit aCaption "number"
layoutItem _ (GXLayoutElementAction aActionName _ aCaption _) = Button aCaption ("onButton" ++ aActionName)
layoutItem _ (GXLayoutElementTextBlock aCaption) = Text aCaption Nothing
layoutItem _ GXLayoutElementImage{} = Image 0 0
layoutItem p GXLayoutElementGrid{} = Grid p
layoutItem _ _ = error "Element type not supported"

gridLayout :: String -> GXLayoutElement -> Layout
gridLayout aPackage (GXLayoutElement _ grid@GXLayoutElementGrid{}) = let GXLayoutElement _ table = head $ gridLayouts grid
                                                                         aRows = rows table
                                                                         aItems = layoutItems aPackage (concat aRows)
                                                                         aHeight = getHeight table
                                                                         aIsVertical = length aRows /= 1 || length (head aRows) == 1 in
                                                                     Layout "line" False False aIsVertical aHeight aItems
gridLayout _ _ = error "Show only be called with grids"

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
