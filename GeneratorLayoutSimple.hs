module GeneratorLayoutSimple (generateLayoutSimple) where

import Templates.AndroidSimpleDataType
import Templates.AndroidLayoutDataType

generateLayoutSimple :: DataSimple -> LayoutList
generateLayoutSimple (DataSimple aPackage _ _ aLayouts) = map (getLayout aPackage) aLayouts

getLayout :: String -> LayoutSimple -> Layout
getLayout aPackage (LayoutSimple aLayoutType aOrientation aLayoutHeight aItems) =
  let aUseScroll = itHasGrid aItems
      aIsVertical = itIsVertical aOrientation
      aLayoutItems = map (layoutItem aPackage) aItems in
    case aLayoutType of
      Main -> Layout "main" aUseScroll True aIsVertical aLayoutHeight aLayoutItems
      Line -> Layout "line" aUseScroll False aIsVertical aLayoutHeight aLayoutItems
  where
    itIsVertical Vertical = True
    itIsVertical Horizontal = False

itHasGrid :: [ItemSimple] -> Bool
itHasGrid = any isGrid
  where
    isGrid (ItemSimple _ GridSimple{}) = True
    isGrid _ = False

layoutItem :: String -> ItemSimple -> Item
layoutItem aPackage (ItemSimple (ItemSimpleBase aId _) specific) = Item (ItemBase aId) (f specific)
  where
    f (TextSimple aText aGravity) = Text aText aGravity
    f (EditSimple aLabel aInputType) = Edit aLabel aInputType
    f (ButtonSimple aText aActionName) = Button aText ("onButton" ++ aActionName)
    f (ImageSimple aWidth aHeight) = Image aWidth aHeight
    f GridSimple = Grid aPackage
