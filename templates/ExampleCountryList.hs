module Templates.ExampleCountryList where

import Templates.AndroidLayoutDataType
import Templates.AndroidMainDataType

countryListLayout :: LayoutList
countryListLayout = [ Layout
      { layoutName = "main"
      , useScroll = True
      , isMain = True
      , isVertical = True
      , layoutHeight = Nothing
      , items =
          [ Item
              { b = ItemBase { Templates.AndroidLayoutDataType.id = "Grid1" }
              , s = Grid { Templates.AndroidLayoutDataType.package = "com.artech.masterwatch.countrylist" }
              }
          ]
      }
  , Layout
      { layoutName = "line"
      , useScroll = False
      , isMain = False
      , isVertical = False
      , layoutHeight = Just 80.0
      , items =
          [ Item
              { b = ItemBase { Templates.AndroidLayoutDataType.id = "Countryflag" }
              , s = Image { width = 100.0 , height = 60.0 }
              }
          , Item
              { b = ItemBase { Templates.AndroidLayoutDataType.id = "Countryname" }
              , s = Text { text = "" , gravity = Nothing }
              }
          ]
      }
  ]

countryListMain :: AndroidMain
countryListMain = AndroidMain
    { Templates.AndroidMainDataType.package = "com.artech.masterwatch.countrylist"
    , dataView = Just "CountryList.Level.Detail"
    , sourceIndex = Just 0
    , hasGrid = True
    , update = Just [ UpdateGrid { Templates.AndroidMainDataType.id = "Grid1" } ]
    , events = Nothing
    }

countryListAdapter :: AndroidAdapter
countryListAdapter = AndroidAdapter
    { packageA = "com.artech.masterwatch.countrylist"
    , bindView =
        [ UpdateImage
            { Templates.AndroidMainDataType.id = "Countryflag" , property = "CountryFlag" , isImage = True }
        , UpdateText
            { Templates.AndroidMainDataType.id = "Countryname" , property = "CountryName" , isText = True }
        ]
    }
