module Main (main) where

import Control.Monad (filterM, forM_)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text.IO qualified as TIO
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Muninn.Parser.JSON (fileToJSON)
import Muninn.Parser (parseFile, parseOdin)
import Muninn.Formatter (formatFile, formatOdin)

data Command
  = Parse FilePath
  | Format FilePath
  | DumpFormat FilePath

commandParser :: Parser Command
commandParser = subparser
  ( command "parse" (info parseCmd (progDesc "Parse an Odin file and output AST as JSON"))
  <> command "format" (info formatCmd (progDesc "Format Odin file(s) in-place"))
  <> command "dump-format" (info dumpFormatCmd (progDesc "Parse and output formatted text to stdout"))
  )
  where
    parseCmd = Parse <$> argument str (metavar "FILE")
    formatCmd = Format <$> argument str (metavar "PATH")
    dumpFormatCmd = DumpFormat <$> argument str (metavar "FILE")

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
  <> progDesc "Odin language static analyzer"
  <> header "muninn - Odin's raven"
  )

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Parse path -> do
      result <- parseFile path
      case result of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ err
          exitFailure
        Right ast -> do
          BL.putStrLn $ encode $ fileToJSON ast

    DumpFormat path -> do
      src <- TIO.readFile path
      case parseOdin path src of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ err
          exitFailure
        Right ast -> TIO.putStr (formatOdin ast)

    Format path -> do
      isDir <- doesDirectoryExist path
      isFile <- doesFileExist path
      if isDir then do
        files <- findOdinFiles path
        forM_ files formatOneFile
      else if isFile then
        formatOneFile path
      else do
        hPutStrLn stderr $ "Error: " ++ path ++ " does not exist"
        exitFailure

formatOneFile :: FilePath -> IO ()
formatOneFile path = do
  result <- formatFile path
  case result of
    Left err -> hPutStrLn stderr $ "Error formatting " ++ path ++ ": " ++ err
    Right formatted -> do
      TIO.writeFile path formatted
      putStrLn $ "Formatted: " ++ path

findOdinFiles :: FilePath -> IO [FilePath]
findOdinFiles dir = do
  entries <- listDirectory dir
  let files = filter (\f -> takeExtension f == ".odin") entries
      filePaths = map (dir </>) files
  subdirs <- filterM doesDirectoryExist (map (dir </>) entries)
  subFiles <- concat <$> mapM findOdinFiles subdirs
  pure $ filePaths ++ subFiles
