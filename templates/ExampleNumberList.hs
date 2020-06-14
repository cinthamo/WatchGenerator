module Templates.ExampleNumberList where

import Templates.AndroidLayoutDataType
import Templates.AndroidMainDataType

numberListLayout :: LayoutList
numberListLayout = [ Layout
      { layoutName = "main"
      , useScroll = True
      , isMain = True
      , isVertical = True
      , layoutHeight = Nothing
      , items =
          [ Item
              { b = ItemBase { Templates.AndroidLayoutDataType.id = "Grid1" }
              , s = Grid { Templates.AndroidLayoutDataType.package = "com.artech.masterwatch.numberlist" }
              }
          ]
      }
  , Layout
      { layoutName = "line"
      , useScroll = False
      , isMain = False
      , isVertical = True
      , layoutHeight = Just 44.0
      , items =
          [ Item
              { b = ItemBase { Templates.AndroidLayoutDataType.id = "_n" }
              , s = Text { text = "" , gravity = Nothing }
              }
          ]
      }
  ]

numberListMain :: AndroidMain
numberListMain = AndroidMain
    { Templates.AndroidMainDataType.package = "com.artech.masterwatch.numberlist"
    , dataView = Just "NumberList.Level.Detail"
    , sourceIndex = Just 1
    , hasGrid = True
    , update = Just [ UpdateGrid { Templates.AndroidMainDataType.id = "Grid1" } ]
    , events = Nothing
    }

numberListAdapter :: AndroidAdapter
numberListAdapter = AndroidAdapter
    { packageA = "com.artech.masterwatch.numberlist"
    , bindView =
        [ UpdateText { Templates.AndroidMainDataType.id = "_n" , property = "N" , isText = True } ]
    }
