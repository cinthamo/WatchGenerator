module MainM where

import Model.GXModelJSONReader

main :: IO ()
main = do
  appModel <- parseApplicationModel "controls" "metadata"
  print appModel
