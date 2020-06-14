{-# LANGUAGE DeriveDataTypeable #-}
module Templates.AndroidMainDataType where

import Data.Typeable
import Data.Data

-- Used to generate the main Activity
data AndroidMain = AndroidMain {
              package :: String,
              dataView :: Maybe String,
              sourceIndex :: Maybe Int,
              hasGrid :: Bool,
              update :: Maybe [UpdateItem],
              events :: Maybe [EventInfo]}
            deriving (Show, Eq, Data, Typeable)

-- Used to generate the recycleradapter onBindViewHolder
data AndroidAdapter = AndroidAdapter {
              packageA :: String,
              bindView :: [UpdateItem]}
            deriving (Show, Eq, Data, Typeable)

data UpdateItem = UpdateText { id :: String, property :: String, isText :: Bool }
                  | UpdateEdit { id :: String, property :: String, isEdit :: Bool }
                  | UpdateImage { id :: String, property :: String, isImage :: Bool }
                  | UpdateGrid { id :: String }
                deriving (Show, Eq, Data, Typeable)

data EventInfo = EventInfo { eventName :: String, actionName :: String }
                 deriving (Show, Eq, Data, Typeable)
