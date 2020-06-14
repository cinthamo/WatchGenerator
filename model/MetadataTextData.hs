module MetadataTextData where

import Model.GXModelBase

textData :: GXAppModel
textData = GXAppModel
  { mainPanelName = "textdata"
  , panelObjects =
      [ GXPanelObject
          { panelName = "TextData"
          , panelDataProvider = "TextData_Level_Detail"
          , dataList =
              [ GXDataElement
                  { dataProvider = "TextData_Level_Detail"
                  , variables =
                      [ GXDataVariableElement
                          { variableName = "Hi" , variableType = GXDataTypeCharacter }
                      ]
                  , attributes = []
                  }
              ]
          , layoutTable =
              GXLayoutElement
                GXLayoutElementBase
                  { controlName = "Maintable"
                  , visible = True
                  , enabled = True
                  , invisibleMode = GXLayoutInvisibleModeKeepSpace
                  }
                GXLayoutElementTable
                  { rows =
                      [ [ GXLayoutElement
                            GXLayoutElementBase
                              { controlName = "Textblock1"
                              , visible = True
                              , enabled = True
                              , invisibleMode = GXLayoutInvisibleModeKeepSpace
                              }
                            GXLayoutElementTextBlock { caption = "Prueba" }
                        ]
                      , [ GXLayoutElement
                            GXLayoutElementBase
                              { controlName = "&hi"
                              , visible = True
                              , enabled = True
                              , invisibleMode = GXLayoutInvisibleModeKeepSpace
                              }
                            GXLayoutElementData
                              { fieldName = "&Hi"
                              , caption = ""
                              , labelPosition = GXLayoutLabelPositionTypeNone
                              , readonly = True
                              , controlType = "Edit"
                              , isPassword = False
                              }
                        ]
                      ]
                  , width = GXLayoutDimensionPercent 100.0
                  , height = GXLayoutDimensionPercent 100.0
                  , tableLayoutName = Nothing
                  }
          }
      ]
  }
