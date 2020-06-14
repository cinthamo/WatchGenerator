{-# LANGUAGE DeriveDataTypeable #-}
module Templates.AndroidLayoutDataType where

import Data.Typeable
import Data.Data

-- LayoutList represent the list of all the Layouts (xml) needed,
-- each Layout will generate a different xml
type LayoutList = [Layout]

data Layout = Layout {
                layoutName :: String,
                useScroll :: Bool,
                isMain :: Bool,
                isVertical :: Bool,
                layoutHeight :: Maybe Double,
                items :: [Item] }
              deriving (Show, Eq, Data, Typeable)

data Item = Item { b :: ItemBase, s :: ItemSpecific }
            deriving (Show, Eq, Data, Typeable)

newtype ItemBase = ItemBase { id :: String }
                   deriving (Show, Eq, Data, Typeable)

data ItemSpecific =
            Text { text :: String, gravity :: Maybe String }
          | Edit { label :: String, inputType :: String }
          | Button { text :: String, onClick :: String }
          | Image { width :: Double, height :: Double }
          | Grid { package :: String }
          deriving (Show, Eq, Data, Typeable)
