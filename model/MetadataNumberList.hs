module MetadataNumberList where

import Model.GXModel

numberList :: GXAppModel
numberList = GXAppModel
  { mainPanelName = "numberlist"
  , panelObjects =
      [ GXPanelObject
          { panelName = "NumberList"
          , panelDataProvider = "NumberList_Level_Detail"
          , dataList =
              [ GXDataElement
                  { dataProvider = "NumberList_Level_Detail"
                  , variables = []
                  , attributes = []
                  }
              , GXDataElement
                  { dataProvider = "NumberList_Level_Detail_Grid1"
                  , variables =
                      [ GXDataVariableElement
                          { variableName = "N" , variableType = GXDataTypeNumeric }
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
                              { controlName = "Grid1"
                              , visible = True
                              , enabled = True
                              , invisibleMode = GXLayoutInvisibleModeKeepSpace
                              }
                            GXLayoutElementGrid
                              { gridLayouts =
                                  [ GXLayoutElement
                                      GXLayoutElementBase
                                        { controlName = "Grid1table"
                                        , visible = True
                                        , enabled = True
                                        , invisibleMode = GXLayoutInvisibleModeKeepSpace
                                        }
                                      GXLayoutElementTable
                                        { rows =
                                            [ [ GXLayoutElement
                                                  GXLayoutElementBase
                                                    { controlName = "&n"
                                                    , visible = True
                                                    , enabled = True
                                                    , invisibleMode = GXLayoutInvisibleModeKeepSpace
                                                    }
                                                  GXLayoutElementData
                                                    { fieldName = "&N"
                                                    , caption = ""
                                                    , labelPosition = GXLayoutLabelPositionTypeNone
                                                    , readonly = True
                                                    , controlType = "Edit"
                                                    }
                                              ]
                                            ]
                                        , width = GXLayoutDimensionPercent 100.0
                                        , height = GXLayoutDimensionPoint 44.0
                                        , tableLayoutName = Just "Layout1"
                                        }
                                  ]
                              , gridDataProvider = "NumberList_Level_Detail_Grid1"
                              , gridDefaultAction = ""
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
