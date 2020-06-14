module Templates.ExampleTextData where

import Templates.AndroidLayoutDataType
import Templates.AndroidMainDataType

textDataLayout :: LayoutList
textDataLayout = [ Layout
      { layoutName = "main"
      , useScroll = False
      , isMain = True
      , isVertical = True
      , layoutHeight = Nothing
      , items =
          [ Item
              { b = ItemBase { Templates.AndroidLayoutDataType.id = "Textblock1" }
              , s = Text { text = "Prueba" , gravity = Nothing }
              }
          , Item
              { b = ItemBase { Templates.AndroidLayoutDataType.id = "_hi" }
              , s = Text { text = "" , gravity = Nothing }
              }
          ]
      }
  ]

textDataMain :: AndroidMain
textDataMain = AndroidMain
    { Templates.AndroidMainDataType.package = "com.artech.masterwatch.textdata"
    , dataView = Just "TextData.Level.Detail"
    , sourceIndex = Nothing
    , hasGrid = False
    , update =
        Just
          [ UpdateText { Templates.AndroidMainDataType.id = "_hi" , property = "Hi" , isText = True } ]
    , events = Nothing
    }
