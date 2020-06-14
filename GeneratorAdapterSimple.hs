module GeneratorAdapterSimple (generateAdapterSimple) where

import Templates.AndroidSimpleDataType
import Templates.AndroidMainDataType
import GeneratorUtilsAndroid
import Data.Maybe

generateAdapterSimple :: DataSimple -> Maybe AndroidAdapter
generateAdapterSimple (DataSimple aPackage _ _ aLayouts) =
  let mainLayouts = head aLayouts
      aHasGrid = itHasGrid $ Templates.AndroidSimpleDataType.items mainLayouts in
   if aHasGrid then Just (AndroidAdapter aPackage (fromJust $ getUpdates $ head $ tail aLayouts)) else Nothing

itHasGrid :: [ItemSimple] -> Bool
itHasGrid = any isGrid
  where
    isGrid (ItemSimple _ GridSimple{}) = True
    isGrid _ = False

getUpdates :: LayoutSimple -> Maybe [UpdateItem]
getUpdates layout = emptyToNothing $ concatMap f (Templates.AndroidSimpleDataType.items layout)
  where
    f (ItemSimple (ItemSimpleBase aId _) GridSimple) = [UpdateGrid aId]
    f (ItemSimple (ItemSimpleBase _ Nothing) _) = []
    f (ItemSimple (ItemSimpleBase aId aProperty) specific) = [g specific aId (fromJust aProperty) True]
    g TextSimple{} = UpdateText
    g EditSimple{} = UpdateEdit
    g ImageSimple{} = UpdateImage
    g _ = error "invalid type for update"
