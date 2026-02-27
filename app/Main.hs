module Main (main) where

import Control.Monad (filterM, forM, forM_, when)
import Data.Aeson (encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Algorithm.Diff (PolyDiff (..), getGroupedDiff)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muninn.Formatter (formatFileWith, formatOdinWith)
import Muninn.Formatter.Config (FmtConfig)
import Muninn.Formatter.ConfigFile (loadConfig, showConfig)
import Muninn.Parser (parseOdin)
import Options.Applicative
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    listDirectory,
  )
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (hPutStrLn, stderr)

data OutputFormat = OutputText | OutputJSON
  deriving (Eq)

data FmtOpts = FmtOpts
  { fmtPath :: Maybe FilePath,
    fmtCheck :: Bool,
    fmtDiff :: Bool,
    fmtDryRun :: Bool,
    fmtOutput :: OutputFormat,
    fmtStdin :: Bool,
    fmtPrintCfg :: Bool
  }

data Command
  = Format FmtOpts

commandParser :: Parser Command
commandParser =
  subparser
    (command "fmt" (info (formatCmd <**> helper) (fullDesc <> progDesc "Format Odin file(s)")))
  where
    formatCmd = Format <$> fmtOptsParser

fmtOptsParser :: Parser FmtOpts
fmtOptsParser =
  FmtOpts
    <$> optional (argument str (metavar "PATH" <> help "File or directory to format"))
    <*> switch (long "check" <> help "Check if files are formatted (exit 1 if not)")
    <*> switch (long "diff" <> help "Show diff of formatting changes")
    <*> switch (long "dry-run" <> help "Write formatted output to stdout instead of modifying files")
    <*> flag OutputText OutputJSON (long "output-json" <> help "Output in JSON format (with --diff)")
    <*> switch (long "stdin" <> help "Read source from stdin, write formatted to stdout")
    <*> switch (long "print-config" <> help "Print resolved configuration and exit")

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> helper)
    ( fullDesc
        <> progDesc "Odin linter and formatter; raven"
    )

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Format fmtOp -> runFormat fmtOp

runFormat :: FmtOpts -> IO ()
runFormat fmtOp = do
  cfg <- resolveConfig fmtOp
  when (fmtPrintCfg fmtOp) $ do
    TIO.putStr (showConfig cfg)
    exitWith ExitSuccess

  if fmtStdin fmtOp
    then runStdin cfg fmtOp
    else case fmtPath fmtOp of
      Nothing -> do
        hPutStrLn stderr "Error: PATH required (or use --stdin)"
        exitFailure
      Just path -> runFiles cfg fmtOp path

resolveConfig :: FmtOpts -> IO FmtConfig
resolveConfig fmtOp = do
  dir <- case fmtPath fmtOp of
    Just p -> do
      isDir <- doesDirectoryExist p
      pure $ if isDir then p else takeDirectory p
    Nothing -> getCurrentDirectory
  loadConfig dir

runStdin :: FmtConfig -> FmtOpts -> IO ()
runStdin cfg fmtOp = do
  src <- TIO.getContents
  case parseOdin "<stdin>" src of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      exitFailure
    Right ast -> do
      let formatted = formatOdinWith cfg (Just src) ast
      if fmtCheck fmtOp
        then
          if formatted /= src
            then do
              hPutStrLn stderr "<stdin>"
              exitWith (ExitFailure 1)
            else pure ()
        else
          if fmtDiff fmtOp
            then case fmtOutput fmtOp of
              OutputText -> TIO.putStr (unifiedDiff "<stdin>" src formatted)
              OutputJSON ->
                BL.putStrLn $
                  encode $
                    Aeson.Array $
                      pure $
                        object ["file" .= ("<stdin>" :: Text), "diff" .= unifiedDiff "<stdin>" src formatted]
            else
              TIO.putStr formatted

runFiles :: FmtConfig -> FmtOpts -> FilePath -> IO ()
runFiles cfg fmtOp path = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  files <-
    if isDir
      then findOdinFiles path
      else
        if isFile
          then pure [path]
          else do
            hPutStrLn stderr $ "Error: " ++ path ++ " does not exist"
            exitFailure

  if fmtCheck fmtOp
    then do
      changed <- forM files (checkOneFile cfg)
      let changedFiles = [f | (f, True) <- changed]
      forM_ changedFiles putStrLn
      when (not (null changedFiles)) $
        exitWith (ExitFailure 1)
    else
      if fmtDiff fmtOp
        then case fmtOutput fmtOp of
          OutputText -> forM_ files (diffOneFile cfg)
          OutputJSON -> do
            diffs <- forM files (diffOneFileJSON cfg)
            let nonEmpty = filter (\(_, d) -> not (T.null d)) diffs
            BL.putStrLn $ encode [object ["file" .= f, "diff" .= d] | (f, d) <- nonEmpty]
        else
          forM_ files (formatOneFile cfg (fmtDryRun fmtOp))

formatOneFile :: FmtConfig -> Bool -> FilePath -> IO ()
formatOneFile cfg dryRun path = do
  result <- formatFileWith cfg path
  case result of
    Left err -> hPutStrLn stderr $ "Error formatting " ++ path ++ ": " ++ err
    Right formatted
      | dryRun -> TIO.putStr formatted
      | otherwise -> do
          TIO.writeFile path formatted
          putStrLn $ "Formatted: " ++ path

checkOneFile :: FmtConfig -> FilePath -> IO (FilePath, Bool)
checkOneFile cfg path = do
  src <- TIO.readFile path
  result <- formatFileWith cfg path
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ path ++ ": " ++ err
      pure (path, False)
    Right formatted -> pure (path, formatted /= src)

diffOneFile :: FmtConfig -> FilePath -> IO ()
diffOneFile cfg path = do
  src <- TIO.readFile path
  result <- formatFileWith cfg path
  case result of
    Left err -> hPutStrLn stderr $ "Error: " ++ path ++ ": " ++ err
    Right formatted ->
      when (formatted /= src) $
        TIO.putStr (unifiedDiff path src formatted)

diffOneFileJSON :: FmtConfig -> FilePath -> IO (FilePath, Text)
diffOneFileJSON cfg path = do
  src <- TIO.readFile path
  result <- formatFileWith cfg path
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ path ++ ": " ++ err
      pure (path, "")
    Right formatted ->
      if formatted == src
        then pure (path, "")
        else pure (path, unifiedDiff path src formatted)

unifiedDiff :: FilePath -> Text -> Text -> Text
unifiedDiff path original formatted =
  let origLines = T.lines original
      fmtLines = T.lines formatted
      diffs = getGroupedDiff origLines fmtLines
      hdr = T.unlines ["--- " <> T.pack path, "+++ " <> T.pack path]
   in hdr <> renderDiffGroups diffs

renderDiffGroups :: [PolyDiff [Text] [Text]] -> Text
renderDiffGroups = T.concat . map renderGroup
  where
    renderGroup (First ls) = T.unlines (map ("-" <>) ls)
    renderGroup (Second ls) = T.unlines (map ("+" <>) ls)
    renderGroup (Both ls _) = T.unlines (map (" " <>) ls)

findOdinFiles :: FilePath -> IO [FilePath]
findOdinFiles dir = do
  entries <- listDirectory dir
  let files = filter (\f -> takeExtension f == ".odin") entries
      filePaths = map (dir </>) files
  subdirs <- filterM doesDirectoryExist (map (dir </>) entries)
  subFiles <- concat <$> mapM findOdinFiles subdirs
  pure $ filePaths ++ subFiles
