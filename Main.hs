module Main where

import Model.GXModelBase
import Model.GXModelConverterAndroid
import Model.GXModelJSONReader
import Templates.ProcessExamples
import Templates.AndroidLayoutDataType
import Templates.AndroidMainDataType
import Generator
import GeneratorPreSimple
import GeneratorLayoutSimple
import GeneratorMainSimple
import GeneratorAdapterSimple
import PrettyHaskell
import Data.Char
import System.Directory
import Text.Show.Pretty (ppShow)

exampleList :: [String]
exampleList = ["Controls", "TextData", "NumberList", "CountryList"]

templates :: IO ()
templates = processExamples

meta :: Int -> IO ()
meta n = let exampleName = exampleList !! n in
  do
    appModel <- parseApplicationModel (map toLower exampleName) "model/metadata"
    putStrLn $ "EXAMPLE " ++ exampleName
    createDirectoryIfMissing True "out"
    writeFile "out/Metadata.hs" $ ppMetadata exampleName appModel
    writeFile "out/Example.hs" $ ppExample exampleName $ generate appModel

metaS :: Int -> IO ()
metaS n = let exampleName = exampleList !! n in
      do
        appModel <- parseApplicationModel (map toLower exampleName) "model/metadata"
        putStrLn $ "EXAMPLE " ++ exampleName
        putStrLn $ ppShow $ generatePreSimple $ convertAndroid appModel

gen :: (GXAppModel -> (LayoutList, AndroidMain, Maybe AndroidAdapter)) -> IO ()
gen generator = do
          genList <- mapM f exampleList
          process genList
       where
          f x = do
                  appModel <- parseApplicationModel (map toLower x) "model/metadata"
                  let (a,b1,c) = generator appModel in
                    return (x,a,b1,c)

mainS :: IO ()
mainS = gen ((\d -> (generateLayoutSimple d, generateMainSimple d, generateAdapterSimple d)).generatePreSimple.convertAndroid)

main :: IO ()
main = gen generate
