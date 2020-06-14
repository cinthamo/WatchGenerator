module Main where

import Model.GXModel
import Model.GXModelJSONReader
import Templates.ProcessExamples
import Templates.AndroidLayoutDataType
import Templates.AndroidMainDataType
import Generator
import GeneratorPreSimple
import GeneratorLayoutSimple
import GeneratorMainSimple
import GeneratorAdapterSimple
import IWatch.AppleGenerator
import IWatch.WatchKitStoryboard
import IWatch.WatchKitStoryboardWriter
import IWatch.SwiftInterfaceControllerClass
import IWatch.SwiftInterfaceControllerClassWriter
import IWatch.GeneratorStoryboardSimple
import IWatch.GeneratorClassesSimple
import PrettyHaskell
import Data.Char
import System.Directory
import Text.Show.Pretty (ppShow)

-- Lista de ejemplos
exampleList :: [String]
exampleList = ["Controls", "TextData", "NumberList", "CountryList"]

-- Procesa de las estructuras finales a archivos
templates :: IO ()
templates = processExamples

-- Graba la estructura de la metadata y estruturas finales (Android)
meta :: Int -> IO ()
meta n = let exampleName = exampleList !! n in
  do
    appModel <- parseApplicationModel (map toLower exampleName) "model/metadata"
    putStrLn $ "EXAMPLE " ++ exampleName
    createDirectoryIfMissing True "out"
    writeFile "out/Metadata.hs" $ ppMetadata exampleName appModel
    writeFile "out/Example.hs" $ ppExample exampleName $ generate appModel

-- Despliega la estrutra intermedia de un ejemplo
metaS :: Int -> IO ()
metaS n = let exampleName = exampleList !! n in
      do
        appModel <- parseApplicationModel (map toLower exampleName) "model/metadata"
        putStrLn $ "EXAMPLE " ++ exampleName
        putStrLn $ ppShow $ generatePreSimple appModel

-- Aplica el generador a la lista de ejemplos y ejecuta los templates (Android)
genAndroid :: (GXAppModel -> (LayoutList, AndroidMain, Maybe AndroidAdapter)) -> IO ()
genAndroid generator = do
          genList <- mapM f exampleList
          process genList
       where
          f x = do
                  appModel <- parseApplicationModel (map toLower x) "model/metadata"
                  let (a,b1,c) = generator appModel in
                    return (x,a,b1,c)

-- Aplica el generador a la lista de ejemplos y ejecuta los templates (iWatch)
genApple :: (GXAppModel -> String -> IO (Document, [InterfaceControllerClass])) -> IO ()
genApple generator = mapM_ f exampleList
  where
    f exampleName = do
      appModel <- parseApplicationModel (map toLower exampleName) "model/metadata"
      (storyboardDoc, classes) <- generator appModel exampleName
      createDirectoryIfMissing True outputDir
      generateStoryboardFile (outputDir ++ "/Interface.storyboard") storyboardDoc
      mapM_ (`generateSwiftClassFile` outputDir) classes where
        outputDir = "iwatch/out/" ++ exampleName

-- Generador con estructura intermedia (iWatch)
mainA :: IO ()
mainA = genApple (\m n -> f (generatePreSimple m) n) where
  f d n = do
    storyboard <- generateStoryboardSimple d n
    return (storyboard, generateClassesSimple d n)

-- Genrador monolitico (iWatch)
mainW :: IO ()
mainW = genApple storyboardAndClassesFromGXModel

-- Generador con estructura intermedia (Android)
mainS :: IO ()
mainS = genAndroid ((\d -> (generateLayoutSimple d, generateMainSimple d, generateAdapterSimple d)).generatePreSimple)

-- Generador monolitico (Android)
main :: IO ()
main = genAndroid generate
