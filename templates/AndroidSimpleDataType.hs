module Templates.AndroidSimpleDataType where

-- DataSimple represent all the informatino needed to generate the files,
-- it is called Simple because it uses haskell data structures and it is all
-- in one place, things that are not ready for string template
data DataSimple = DataSimple {
                package :: String,
                dataView :: Maybe String,
                sourceIndex :: Maybe Int,
                layouts :: [LayoutSimple] }
              deriving (Show, Eq)

data LayoutSimpleType = Main | Line
              deriving (Show, Eq)

data LayoutSimpleOrientation = Vertical | Horizontal
              deriving (Show, Eq)

data LayoutSimple = LayoutSimple {
                layoutType :: LayoutSimpleType,
                orientation :: LayoutSimpleOrientation,
                layoutHeight :: Maybe Double,
                items :: [ItemSimple] }
              deriving (Show, Eq)

data ItemSimple = ItemSimple {
                b :: ItemSimpleBase,
                s :: ItemSimpleSpecific }
              deriving (Show, Eq)

data ItemSimpleBase = ItemSimpleBase {
                id :: String,
                property :: Maybe String }
              deriving (Show, Eq)

data ItemSimpleSpecific =
            TextSimple { text :: String, gravity :: Maybe String }
          | EditSimple { label :: String, inputType :: String }
          | ButtonSimple { text :: String, actionName :: String  }
          | ImageSimple { width :: Double, height :: Double }
          | GridSimple
          deriving (Show, Eq)
