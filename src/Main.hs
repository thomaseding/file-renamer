{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_, when)
import Data.Char (toLower)
import Data.List (unfoldr)
import System.Directory (listDirectory, renameFile)
import System.Environment (getArgs)
import System.FilePath (takeExtension, (</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Random.Shuffle (shuffleM)

data Options = Options
  { shuffle :: Bool
  , ext :: Maybe String
  , extImage :: Bool
  , dir :: Maybe FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { shuffle = False
  , ext = Nothing
  , extImage = False
  , dir = Nothing
  }

parseArgs :: [String] -> Options -> IO Options
parseArgs [] opts = return opts
parseArgs ("--help":_) _ = do
  TIO.putStrLn helpText
  return defaultOptions
parseArgs ("--shuffle":xs) opts = parseArgs xs $ opts {shuffle = True}
parseArgs ("--ext":x:xs) opts = parseArgs xs $ opts {ext = Just x}
parseArgs ("--ext-image":xs) opts = parseArgs xs $ opts {extImage = True}
parseArgs ("--dir":x:xs) opts = parseArgs xs $ opts {dir = Just x}
parseArgs _ _ = do
  putStrLn "Invalid arguments. Use --help for usage information."
  return defaultOptions

helpText :: T.Text
helpText = T.unlines
  [ "Usage: file-renamer [OPTIONS]"
  , "Renames files in a flat directory sequentially with a number starting at 0."
  , ""
  , "Options:"
  , "  --help          Display this help message"
  , "  --shuffle       Shuffle the files before renaming"
  , "  --ext EXT       Filter files by one or more extensions (case insensitive), separate by commas (e.g., .txt,.pdf)"
  , "  --ext-image     Filter common image formats (overrides --ext if both are specified)"
  , "  --dir DIR       Specify the directory to process (required)"
  ]

imageExtensions :: [String]
imageExtensions = [".jpg", ".jpeg", ".png", ".gif", ".bmp", ".tiff", ".svg"]

main :: IO ()
main = do
  args <- getArgs
  opts <- parseArgs args defaultOptions

  case dir opts of
    Nothing -> putStrLn "Error: --dir option is required. Use --help for usage information."
    Just folder -> do
      allFiles <- listDirectory folder
      let filteredFiles = filterFiles opts allFiles

      if null allFiles
        then putStrLn "Warning: No files present in the specified directory."
        else when (null filteredFiles) $
          putStrLn "Warning: No files present after applying filters."

      filesToRename <- if shuffle opts then shuffleM filteredFiles else return filteredFiles

      forM_ (zip filesToRename [0 :: Int ..]) $ \(file, i) -> do
        let oldPath = folder </> file
            newPath = folder </> show i ++ takeExtension file
        renameFile oldPath newPath

filterFiles :: Options -> [FilePath] -> [FilePath]
filterFiles opts files =
  let extFilter = maybe (const True) (\x -> (`elem` map (map toLower) (splitExtensions x)) . map toLower . takeExtension) (ext opts)
      imageFilter = if extImage opts then (\file -> any (== map toLower (takeExtension file)) imageExtensions) else const True
  in filter (\file -> extFilter file && imageFilter file) files


splitExtensions :: String -> [String]
splitExtensions = unfoldr $ \s ->
  if null s
    then Nothing
    else case break (== ',') s of
      (ext, rest) -> Just (ext, drop 1 rest)
