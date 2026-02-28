module Main (main) where

import Control.Monad (filterM)
import Data.Aeson (Value (..), eitherDecode, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Muninn.Parser (parseFile)
import Muninn.Parser.JSON (fileToJSON)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeExtension, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

main :: IO ()
main = do
    odinRoot <- requireEnv "ODIN_SRC_TESTDATA"
    olsRoot <- requireEnv "OLS_SRC_TESTDATA"
    requireGoldenDir "test/parser/golden-stdlib"
    requireGoldenDir "test/parser/golden-ols"
    stdlibTests <- mkGoldenTests "stdlib" odinRoot "test/parser/golden-stdlib"
    olsTests <- mkGoldenTests "ols" olsRoot "test/parser/golden-ols"
    defaultMain $
        testGroup
            "Muninn"
            [ stdlibTests
            , olsTests
            , reservedPkgTests odinRoot
            ]

requireEnv :: String -> IO String
requireEnv name = do
    val <- lookupEnv name
    case val of
        Just v -> pure v
        Nothing -> do
            hPutStrLn stderr $ name ++ " not set. Run inside nix develop."
            exitFailure

requireGoldenDir :: FilePath -> IO ()
requireGoldenDir dir = do
    exists <- doesDirectoryExist dir
    if exists
        then pure ()
        else do
            hPutStrLn stderr $ "Golden directory missing: " ++ dir ++ "\nRun: nix run .#testgen-parser"
            exitFailure

isReservedPkgGolden :: FilePath -> Bool
isReservedPkgGolden p =
    ("base" </> "builtin" </> "builtin.json") `isSuffixOf` p
        || ("base" </> "intrinsics" </> "intrinsics.json") `isSuffixOf` p

mkGoldenTests :: String -> FilePath -> FilePath -> IO TestTree
mkGoldenTests name srcRoot goldenDir = do
    goldens <- filter (not . isReservedPkgGolden) <$> findGoldenFiles goldenDir
    tests <- mapM (mkGoldenTest srcRoot goldenDir) goldens
    pure $ testGroup name tests

mkGoldenTest :: FilePath -> FilePath -> FilePath -> IO TestTree
mkGoldenTest odinRoot goldenDir relPath = do
    let goldenPath = goldenDir </> relPath
        odinPath = odinRoot </> dropExtension relPath ++ ".odin"
    pure $ testCase relPath $ do
        goldenBytes <- BL.readFile goldenPath
        expected <- case eitherDecode goldenBytes :: Either String Value of
            Left err -> do _ <- assertFailure ("failed to parse golden JSON: " ++ err); pure Null
            Right v -> pure v
        result <- parseFile odinPath
        case result of
            Left err -> assertFailure err
            Right ast -> assertEqual "" expected (toJSON (fileToJSON ast))

reservedPkgTests :: FilePath -> TestTree
reservedPkgTests odinRoot =
    testGroup
        "Reserved package names"
        [ testCase "Odin rejects builtin (golden has empty decls)" $ do
            goldenBytes <- BL.readFile "test/parser/golden-stdlib/base/builtin/builtin.json"
            case eitherDecode goldenBytes :: Either String Value of
                Left err -> assertFailure err
                Right val -> assertEmptyDecls "builtin" val
        , testCase "Odin rejects intrinsics (golden has empty decls)" $ do
            goldenBytes <- BL.readFile "test/parser/golden-stdlib/base/intrinsics/intrinsics.json"
            case eitherDecode goldenBytes :: Either String Value of
                Left err -> assertFailure err
                Right val -> assertEmptyDecls "intrinsics" val
        , testCase "Muninn parses builtin with decls" $ do
            result <- parseFile (odinRoot </> "base/builtin/builtin.odin")
            case result of
                Left err -> assertFailure err
                Right ast -> do
                    let json = toJSON (fileToJSON ast)
                    assertNonEmptyDecls "builtin" json
        , testCase "Muninn parses intrinsics with decls" $ do
            result <- parseFile (odinRoot </> "base/intrinsics/intrinsics.odin")
            case result of
                Left err -> assertFailure err
                Right ast -> do
                    let json = toJSON (fileToJSON ast)
                    assertNonEmptyDecls "intrinsics" json
        ]

assertEmptyDecls :: String -> Value -> IO ()
assertEmptyDecls pkg (Object obj) = do
    case KM.lookup (Key.fromString "decls") obj of
        Just (Array arr) ->
            assertEqual (pkg ++ ": Odin parser should produce empty decls") 0 (length arr)
        _ -> assertFailure (pkg ++ ": missing decls key")
assertEmptyDecls pkg _ = assertFailure (pkg ++ ": expected object")

assertNonEmptyDecls :: String -> Value -> IO ()
assertNonEmptyDecls pkg (Object obj) = do
    case KM.lookup (Key.fromString "decls") obj of
        Just (Array arr) ->
            assertBool (pkg ++ ": Muninn should produce non-empty decls") (length arr > 0)
        _ -> assertFailure (pkg ++ ": missing decls key")
assertNonEmptyDecls pkg _ = assertFailure (pkg ++ ": expected object")

findGoldenFiles :: FilePath -> IO [FilePath]
findGoldenFiles dir = do
    entries <- listDirectory dir
    let goldens = filter (\f -> takeExtension f == ".json") entries
    subdirs <- filterM doesDirectoryExist (map (dir </>) entries)
    subFiles <-
        concat
            <$> mapM
                ( \d -> do
                    fs <- findGoldenFiles d
                    let rel = takeFileName d
                    pure $ map (rel </>) fs
                )
                subdirs
    pure $ goldens ++ subFiles
