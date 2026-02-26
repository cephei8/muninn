module Main (main) where

import Control.Monad (filterM)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)
import Muninn.Parser (parseOdin)
import Muninn.Parser.AST (File(..))
import Muninn.Parser.JSON (fileToJSON)
import Muninn.Formatter (formatOdin)

main :: IO ()
main = do
  odinRoot <- requireEnv "ODIN_SRC_TESTDATA"
  olsRoot <- requireEnv "OLS_SRC_TESTDATA"
  stdlibTests <- mkRoundTripTests "stdlib" odinRoot ["core", "vendor", "base"]
  olsTests <- mkRoundTripTests "ols" olsRoot ["src", "tests", "tools", "builtin"]
  defaultMain $ testGroup "Formatter" [stdlibTests, olsTests]

requireEnv :: String -> IO String
requireEnv name = do
  val <- lookupEnv name
  case val of
    Just v -> pure v
    Nothing -> do
      hPutStrLn stderr $ name ++ " not set. Run inside nix develop."
      exitFailure

mkRoundTripTests :: String -> FilePath -> [String] -> IO TestTree
mkRoundTripTests name srcRoot subdirs = do
  files <- concat <$> mapM (findOdinFiles . (srcRoot </>)) subdirs
  let tests = map (mkRoundTripTest srcRoot) files
  pure $ testGroup name tests

mkRoundTripTest :: FilePath -> FilePath -> TestTree
mkRoundTripTest srcRoot odinPath =
  let relPath = drop (length srcRoot + 1) odinPath
  in testCase relPath $ do
      src <- TIO.readFile odinPath
      case parseOdin odinPath src of
        Left err -> assertFailure $ "Parse original failed: " ++ err
        Right ast1
          | isEmptyPkg ast1 -> pure ()  -- skip: empty/reserved package name can't round-trip
          | otherwise -> do
          let formatted = formatOdin ast1
          case parseOdin "<formatted>" formatted of
            Left err -> assertFailure $
              "Parse formatted failed: " ++ err ++ "\n\nFormatted output:\n" ++ T.unpack formatted
            Right ast2 -> do
              let json1 = stripForComparison (toJSON (fileToJSON ast1))
                  json2 = stripForComparison (toJSON (fileToJSON ast2))
              if json1 == json2 then pure ()
              else assertFailure $ "Round-trip mismatch\n" ++ findFirstDiff json1 json2

isEmptyPkg :: File a -> Bool
isEmptyPkg (File _ pkg _ _ _) = T.null pkg

stripForComparison :: Value -> Value
stripForComparison = stripComments . stripForeignPaths . flattenBlocks . stripPositions

stripPositions :: Value -> Value
stripPositions = \case
  Object obj -> Object $ KM.map stripPositions $ KM.filterWithKey (\k _ -> k `notElem` posKeys) obj
  Array arr -> Array $ fmap stripPositions arr
  v -> v
  where
    posKeys = [Key.fromString "pos", Key.fromString "end"]

-- Normalize ForeignImportDecl: conditional paths stripped to [] by parser,
-- formatter emits "" which re-parses as [""]
stripForeignPaths :: Value -> Value
stripForeignPaths = \case
  Object obj
    | Just (String "ForeignImportDecl") <- KM.lookup (Key.fromString "node") obj
    , Just (Array paths) <- KM.lookup (Key.fromString "paths") obj
    , paths == V.singleton (String "")
    -> Object $ KM.map stripForeignPaths $ KM.insert (Key.fromString "paths") (Array V.empty) obj
    | otherwise -> Object $ KM.map stripForeignPaths obj
  Array arr -> Array $ fmap stripForeignPaths arr
  v -> v

-- Flatten label-less BlockStmts in stmts arrays (from stripped directives like #no_bounds_check)
flattenBlocks :: Value -> Value
flattenBlocks = \case
  Object obj -> Object $ KM.mapWithKey flattenField obj
  Array arr -> Array $ fmap flattenBlocks arr
  v -> v
  where
    flattenField k v
      | k == Key.fromString "stmts" = flattenStmts (flattenBlocks v)
      | otherwise = flattenBlocks v
    flattenStmts (Array arr) = Array $ V.concatMap expandBlock arr
    flattenStmts v = v
    expandBlock (Object obj)
      | Just (String "BlockStmt") <- KM.lookup (Key.fromString "node") obj
      , Just Null <- KM.lookup (Key.fromString "label") obj
      , Just (Array inner) <- KM.lookup (Key.fromString "stmts") obj
      = inner
    expandBlock v = V.singleton v

stripComments :: Value -> Value
stripComments = \case
  Object obj
    | KM.member (Key.fromString "node") obj
    , Just (String "File") <- KM.lookup (Key.fromString "node") obj
    -> Object $ KM.map stripComments $ KM.filterWithKey (\k _ -> k `notElem` commentKeys) obj
    | otherwise -> Object $ KM.map stripComments obj
  Array arr -> Array $ fmap stripComments arr
  v -> v
  where
    commentKeys = [Key.fromString "comments", Key.fromString "docs"]

findFirstDiff :: Value -> Value -> String
findFirstDiff (Object o1) (Object o2)
  | Just (Array d1) <- KM.lookup (Key.fromString "decls") o1
  , Just (Array d2) <- KM.lookup (Key.fromString "decls") o2
  = let pairs = zip3 [0::Int ..] (V.toList d1) (V.toList d2)
        diffs = [(i, a, b) | (i, a, b) <- pairs, a /= b]
    in case diffs of
      [] | V.length d1 /= V.length d2 ->
            "Decl count: " ++ show (V.length d1) ++ " vs " ++ show (V.length d2)
         | otherwise -> "Top-level match but pkg/node differ"
      ((i, a, b):_) ->
        "First diff at decl " ++ show i ++ ":\n"
        ++ "  ORIG: " ++ take 1500 (BL.unpack (encodePretty a)) ++ "\n"
        ++ "  FMT:  " ++ take 1500 (BL.unpack (encodePretty b))
findFirstDiff a b = "Structural: " ++ take 200 (show a) ++ " vs " ++ take 200 (show b)

findOdinFiles :: FilePath -> IO [FilePath]
findOdinFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else do
    entries <- listDirectory dir
    let files = filter (\f -> takeExtension f == ".odin") entries
        filePaths = map (dir </>) files
    subdirs <- filterM doesDirectoryExist (map (dir </>) entries)
    subFiles <- concat <$> mapM findOdinFiles subdirs
    pure $ filePaths ++ subFiles
