module MetadataCountryList where

import Model.GXModelBase

countryList :: GXAppModel
countryList = GXAppModel
  { mainPanelName = "countrylist"
  , panelObjects =
      [ GXPanelObject
          { panelName = "CountryList"
          , panelDataProvider = ""
          , dataList =
              [ GXDataElement
                  { dataProvider = "CountryList_Level_Detail_Grid1"
                  , variables = []
                  , attributes =
                      [ GXDataAttributeElement
                          { attributeName = "adbb33c9-0906-4971-833c-998de27e0676-CountryId"
                          , attributeType = GXDataTypeNumeric
                          }
                      , GXDataAttributeElement
                          { attributeName =
                              "adbb33c9-0906-4971-833c-998de27e0676-CountryFlag"
                          , attributeType = GXDataTypeImage
                          }
                      , GXDataAttributeElement
                          { attributeName =
                              "adbb33c9-0906-4971-833c-998de27e0676-CountryName"
                          , attributeType = GXDataTypeCharacter
                          }
                      ]
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
                                                    { controlName = "Countryflag"
                                                    , visible = True
                                                    , enabled = True
                                                    , invisibleMode = GXLayoutInvisibleModeKeepSpace
                                                    }
                                                  GXLayoutElementData
                                                    { fieldName = "CountryFlag"
                                                    , caption = ""
                                                    , labelPosition = GXLayoutLabelPositionTypeNone
                                                    , readonly = True
                                                    , controlType = "Image"
                                                    , isPassword = False
                                                    }
                                              , GXLayoutElement
                                                  GXLayoutElementBase
                                                    { controlName = "Countryname"
                                                    , visible = True
                                                    , enabled = True
                                                    , invisibleMode = GXLayoutInvisibleModeKeepSpace
                                                    }
                                                  GXLayoutElementData
                                                    { fieldName = "CountryName"
                                                    , caption = ""
                                                    , labelPosition = GXLayoutLabelPositionTypeNone
                                                    , readonly = True
                                                    , controlType = "Edit"
                                                    , isPassword = False
                                                    }
                                              ]
                                            ]
                                        , width = GXLayoutDimensionPercent 100.0
                                        , height = GXLayoutDimensionPoint 80.0
                                        , tableLayoutName = Just "Layout1"
                                        }
                                  ]
                              , gridDataProvider = "CountryList_Level_Detail_Grid1"
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