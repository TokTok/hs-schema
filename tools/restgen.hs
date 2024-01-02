{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad               (when)
import qualified Data.Schema.OpenAPI.Haskell as Haskell
import           Data.Schema.OpenAPI.Types   (Path (pathPatch))
import qualified Data.Schema.OpenAPI.Types   as OpenAPI
import qualified Data.Text.IO                as Text
import           Data.Yaml                   (Value (..), decodeFileThrow,
                                              encodeFile)
import           System.Environment          (getArgs)
import           System.Exit                 (exitFailure)

debug :: Bool
debug = False

loadSpec :: FilePath -> IO Value
loadSpec = decodeFileThrow

parseSpec :: Value -> IO OpenAPI.Spec
parseSpec json =
  case OpenAPI.parseSpec json of
    Left err -> fail err
    Right ok -> return ok

main :: IO ()
main = do
  getArgs >>= \case
    [file, _output] -> do
      putStrLn $ "Loading " <> file <> ".yaml"
      json <- OpenAPI.resolveRefs <$> loadSpec (file <> ".yaml")
      when debug $ do
        putStrLn $ "Writing expanded spec to " <> file <> ".expanded.yaml"
        encodeFile (file <> ".expanded.yaml") $ OpenAPI.removeNulls json
        putStrLn "Parsing Spec"
      spec <- parseSpec json
      when debug $ do
        putStrLn "Pretty-printing spec"
        encodeFile (file <> ".parsed.yaml") $ OpenAPI.removeNulls spec
      case Haskell.toHaskell spec "/repos/{owner}/{repo}" pathPatch of
        Nothing -> putStrLn "Failed to generate Haskell code"
        Just ok -> Text.putStrLn ok
    _ -> do
      putStrLn "Usage: restgen <ghes> <output-dir>"
      exitFailure
