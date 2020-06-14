module GeneratorMainSimple (generateMainSimple) where

import Templates.AndroidSimpleDataType
import Templates.AndroidMainDataType
import GeneratorUtils
import Data.Maybe

generateMainSimple :: DataSimple -> AndroidMain
generateMainSimple (DataSimple aPackage aDataView aSourceIndex aLayouts) =
  let mainLayouts = head aLayouts
      aHasGrid = itHasGrid $ Templates.AndroidSimpleDataType.items mainLayouts in
   AndroidMain aPackage aDataView aSourceIndex aHasGrid (getUpdates mainLayouts) (getEvents mainLayouts)

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

getEvents :: LayoutSimple -> Maybe [EventInfo]
getEvents layout = emptyToNothing $ concatMap f (Templates.AndroidSimpleDataType.items layout)
  where
    f (ItemSimple _ (ButtonSimple _ aActionName)) = [EventInfo ("onButton" ++ aActionName) aActionName]
    f _ = []
