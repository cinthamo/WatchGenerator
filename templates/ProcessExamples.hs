module Templates.ProcessExamples (processExamples, process) where

import Templates.ExampleControls
import Templates.ExampleTextData
import Templates.ExampleNumberList
import Templates.ExampleCountryList
import Templates.AndroidMainDataType
import Templates.AndroidLayoutDataType
import Text.StringTemplate
import Text.StringTemplate.GenericStandard()
import System.Directory

renderExample :: STGroup String -> String -> AndroidMain -> LayoutList -> Maybe AndroidAdapter -> IO ()
renderExample templates directory dMain dLayout mdAdapter = do
  let outDirectory = "templates/out/" ++ directory
  createDirectoryIfMissing True outDirectory
  renderFile "AndroidMain" templates dMain (outDirectory ++ "/MainNew.java")
  mapM_ (\d -> renderFile "AndroidLayout" templates d (outDirectory ++ "/" ++ layoutName d ++ ".xml")) dLayout
  case mdAdapter of
    Just dAdapter -> do
      renderFile "AndroidRecyclerAdapter" templates dAdapter (outDirectory ++ "/MyRecyclerAdapter.java")
      renderFile "AndroidRecyclerView" templates dAdapter (outDirectory ++ "/MyRecyclerView.java")
    Nothing -> return ()

-- apply a template using a data and write it to a file
renderFile :: ToSElem a => String -> STGroup String -> a -> String -> IO ()
renderFile templateName templates d outFileName = do
  let Just t = getStringTemplate templateName templates
  let str = render $ setAttribute "d" d t
  writeFile outFileName str

processExamples :: IO ()
processExamples = do
  templates <- directoryGroup "templates" :: IO (STGroup String)
  renderExample templates "Controls" controlsMain controlsLayout Nothing
  renderExample templates "TextData" textDataMain textDataLayout Nothing
  renderExample templates "NumberList" numberListMain numberListLayout (Just numberListAdapter)
  renderExample templates "CountryList" countryListMain countryListLayout (Just countryListAdapter)

process :: [(String, LayoutList, AndroidMain, Maybe AndroidAdapter)] -> IO ()
process examples = do
  templates <- directoryGroup "templates" :: IO (STGroup String)
  mapM_ (\(n, layout, main, adap) -> renderExample templates n main layout adap) examples
