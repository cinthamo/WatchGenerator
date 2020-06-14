module Templates.ExampleControls where

import Templates.AndroidLayoutDataType
import Templates.AndroidMainDataType

controlsLayout :: LayoutList
controlsLayout = [ Layout
      { layoutName = "main"
      , useScroll = False
      , isMain = True
      , isVertical = True
      , layoutHeight = Nothing
      , items =
          [ Item
              { b = ItemBase { Templates.AndroidLayoutDataType.id = "_num" }
              , s =
                  Edit
                    { inputType = "number"
                    , label = "Number"
                    , labelOrientation = "vertical"
                    , labelBefore = True
                    , labelAfter = False
                    }
              }
          , Item
              { b = ItemBase { Templates.AndroidLayoutDataType.id = "Click" }
              , s = Button { text = "Click" , onClick = "onButtonClick" }
              }
          ]
      }
  ]

controlsMain :: AndroidMain
controlsMain = AndroidMain
    { Templates.AndroidMainDataType.package = "com.artech.masterwatch.controls"
    , dataView = Just "Controls.Level.Detail"
    , sourceIndex = Nothing
    , hasGrid = False
    , update =
        Just
          [ UpdateEdit { Templates.AndroidMainDataType.id = "_num" , property = "Num" , isEdit = True } ]
    , events =
        Just
          [ EventInfo { eventName = "onButtonClick" , actionName = "Click" }
          ]
    }
