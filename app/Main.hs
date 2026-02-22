module Main (main) where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BL
import Options.Applicative
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Muninn.Parser.JSON (fileToJSON)
import Muninn.Parser (parseFile)

data Command
  = Parse FilePath

commandParser :: Parser Command
commandParser = subparser
  ( command "parse" (info parseCmd (progDesc "Parse an Odin file and output AST as JSON"))
  )
  where
    parseCmd = Parse <$> argument str (metavar "FILE")

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
