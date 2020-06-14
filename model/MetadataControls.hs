module MetadataControls where

import Model.GXModelBase

controls :: GXAppModel
controls = GXAppModel
  { mainPanelName = "controls"
  , panelObjects =
      [ GXPanelObject
          { panelName = "Controls"
          , panelDataProvider = "Controls_Level_Detail"
          , dataList =
              [ GXDataElement
                  { dataProvider = "Controls_Level_Detail"
                  , variables =
                      [ GXDataVariableElement
                          { variableName = "Num" , variableType = GXDataTypeNumeric }
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
                              { controlName = "&num"
                              , visible = True
                              , enabled = True
                              , invisibleMode = GXLayoutInvisibleModeKeepSpace
                              }
                            GXLayoutElementData
                              { fieldName = "&Num"
                              , caption = "Number"
                              , labelPosition = GXLayoutLabelPositionTypeTop
                              , readonly = False
                              , controlType = "Edit"
                              , isPassword = False
                              }
                        ]
                      , [ GXLayoutElement
                            GXLayoutElementBase
                              { controlName = "Click"
                              , visible = True
                              , enabled = True
                              , invisibleMode = GXLayoutInvisibleModeKeepSpace
                              }
                            GXLayoutElementAction
                              { actionName = "Click"
                              , size = ( 0.0 , 0.0 )
                              , caption = "Click"
                              , imageName = ""
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
