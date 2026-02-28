module Main (main) where

import Control.Monad (filterM)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List (isInfixOf, isSuffixOf)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Muninn.Formatter (formatOdinWith)
import Muninn.Formatter.Config (FmtConfig (..), IndentStyle (..), defaultFmtConfig)
import Muninn.Formatter.ConfigFile (parseConfigToml)
import Muninn.Parser (parseOdin)
import Muninn.Parser.AST (File (..))
import Muninn.Parser.JSON (fileToJSON)
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

main :: IO ()
main = do
    odinRoot <- requireEnv "ODIN_SRC_TESTDATA"
    olsRoot <- requireEnv "OLS_SRC_TESTDATA"
    stdlibTests <- mkRoundTripTests "stdlib" odinRoot ["core", "vendor", "base", "tests"]
    olsTests <- mkRoundTripTests "ols" olsRoot ["src", "tests", "tools", "builtin"]
    exePath <- findExe
    defaultMain $
        testGroup
            "Formatter"
            [ stdlibTests
            , olsTests
            , configTests
            , pragmaTests
            , commentTests
            , configFileTests
            , cliTests exePath
            ]

requireEnv :: String -> IO String
requireEnv name = do
    val <- lookupEnv name
    case val of
        Just v -> pure v
        Nothing -> do
            hPutStrLn stderr $ name ++ " not set. Run inside nix develop."
            exitFailure

findExe :: IO FilePath
findExe = do
    cwd <- getCurrentDirectory
    (code, out, _err) <- readProcessWithExitCode "cabal" ["list-bin", "muninn"] ""
    case code of
        ExitSuccess -> pure (filter (/= '\n') out)
        _ -> pure (cwd </> "dist-newstyle/build/muninn")

isReservedPkgFile :: FilePath -> Bool
isReservedPkgFile p =
    ("base" </> "builtin" </> "builtin.odin") `isSuffixOf` p
        || ("base" </> "intrinsics" </> "intrinsics.odin") `isSuffixOf` p

mkRoundTripTests :: String -> FilePath -> [String] -> IO TestTree
mkRoundTripTests name srcRoot subdirs = do
    files <- filter (not . isReservedPkgFile) . concat <$> mapM (findOdinFiles . (srcRoot </>)) subdirs
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
                    | isEmptyPkg ast1 -> pure ()
                    | otherwise -> do
                        let formatted = formatOdinWith defaultFmtConfig (Just src) ast1
                        case parseOdin "<formatted>" formatted of
                            Left err ->
                                assertFailure $
                                    "Parse formatted failed: " ++ err ++ "\n\nFormatted output:\n" ++ T.unpack formatted
                            Right ast2 -> do
                                let json1 = stripForComparison (toJSON (fileToJSON ast1))
                                    json2 = stripForComparison (toJSON (fileToJSON ast2))
                                if json1 == json2
                                    then pure ()
                                    else assertFailure $ "Round-trip mismatch\n" ++ findFirstDiff json1 json2

isEmptyPkg :: File a -> Bool
isEmptyPkg (File _ pkg _ _ _) = T.null pkg

configTests :: TestTree
configTests =
    testGroup
        "Config options"
        [ testGroup
            "Indent style & width"
            [ testCase "Tabs (default)" $ do
                let src = "package test\n\nfoo :: proc() {\n\tx := 1\n}\n"
                case parseOdin "<test>" src of
                    Left err -> assertFailure err
                    Right ast -> do
                        let out = formatOdinWith defaultFmtConfig (Just src) ast
                        assertBool
                            "Expected tab indentation"
                            (any (T.isPrefixOf "\t") (T.lines out))
                        assertBool
                            "Expected no space indentation"
                            (not $ any (T.isPrefixOf "    ") (T.lines out))
            , testCase "Tabs" $ do
                let cfg = defaultFmtConfig{cfgIndentStyle = Tabs}
                    src = "package test\n\nfoo :: proc() {\n\tx := 1\n}\n"
                case parseOdin "<test>" src of
                    Left err -> assertFailure err
                    Right ast -> do
                        let out = formatOdinWith cfg (Just src) ast
                        assertBool
                            "Expected tab indentation"
                            (any (T.isPrefixOf "\t") (T.lines out))
            , testCase "Spaces, width=2" $ do
                let cfg = defaultFmtConfig{cfgIndentStyle = Spaces, cfgIndentWidth = 2}
                    src = "package test\n\nfoo :: proc() {\n  x := 1\n}\n"
                case parseOdin "<test>" src of
                    Left err -> assertFailure err
                    Right ast -> do
                        let out = formatOdinWith cfg (Just src) ast
                        assertBool
                            "Expected 2-space indentation"
                            (any (T.isPrefixOf "  " .&&. (not . T.isPrefixOf "   ")) (T.lines out))
            , testCase "Spaces, width=4" $ do
                let cfg = defaultFmtConfig{cfgIndentStyle = Spaces, cfgIndentWidth = 4}
                    src = "package test\n\nfoo :: proc() {\n    x := 1\n}\n"
                case parseOdin "<test>" src of
                    Left err -> assertFailure err
                    Right ast -> do
                        let out = formatOdinWith cfg (Just src) ast
                        assertBool
                            "Expected 4-space indentation"
                            (any (T.isPrefixOf "    ") (T.lines out))
            , testCase "Spaces, width=8" $ do
                let cfg = defaultFmtConfig{cfgIndentStyle = Spaces, cfgIndentWidth = 8}
                    src = "package test\n\nfoo :: proc() {\n        x := 1\n}\n"
                case parseOdin "<test>" src of
                    Left err -> assertFailure err
                    Right ast -> do
                        let out = formatOdinWith cfg (Just src) ast
                        assertBool
                            "Expected 8-space indentation"
                            (any (T.isPrefixOf "        ") (T.lines out))
            , testCase "Nested indentation" $ do
                let cfg = defaultFmtConfig{cfgIndentStyle = Spaces, cfgIndentWidth = 2}
                    src = "package test\n\nfoo :: proc() {\n  if true {\n    x := 1\n  }\n}\n"
                case parseOdin "<test>" src of
                    Left err -> assertFailure err
                    Right ast -> do
                        let out = formatOdinWith cfg (Just src) ast
                            ls = T.lines out
                        assertBool
                            "Expected nested indentation at 4 spaces"
                            (any (T.isPrefixOf "    ") ls)
            ]
        , testGroup
            "Newline limit"
            [ testCase "Formatter produces canonical spacing (no excess blank lines)" $ do
                let src = "package test\n\n\n\nfoo :: 1\n\n\n\nbar :: 2\n"
                case parseOdin "<test>" src of
                    Left err -> assertFailure err
                    Right ast -> do
                        let out = formatOdinWith defaultFmtConfig (Just src) ast
                        assertBool
                            "No triple newlines in output"
                            (not (T.isInfixOf "\n\n\n" out))
            , testCase "Blank line gaps preserved from source" $ do
                let src1 = "package test\n\nfoo :: 1\nbar :: 2\n"
                    src2 = "package test\n\nfoo :: 1\n\nbar :: 2\n"
                case (parseOdin "<test>" src1, parseOdin "<test>" src2) of
                    (Right ast1, Right ast2) -> do
                        let out1 = formatOdinWith defaultFmtConfig (Just src1) ast1
                            out2 = formatOdinWith defaultFmtConfig (Just src2) ast2
                        assertBool
                            "No blank line when source has none"
                            (not (T.isInfixOf "foo :: 1\n\n" out1))
                        assertBool
                            "Blank line preserved when source has one"
                            (T.isInfixOf "foo :: 1\n\n" out2)
                    _ -> assertFailure "Parse failed"
            , testCase "Multiple blank lines collapsed to one" $ do
                let src = "package test\n\n\n\n\nfoo :: 1\n\n\n\n\nbar :: 2\n"
                case parseOdin "<test>" src of
                    Left err -> assertFailure err
                    Right ast -> do
                        let out = formatOdinWith defaultFmtConfig (Just src) ast
                        assertBool
                            "Has blank line between decls"
                            (T.isInfixOf "foo :: 1\n\nbar :: 2" out)
                        assertBool
                            "No triple newlines"
                            (not (T.isInfixOf "\n\n\n" out))
            , testCase "Config newline_limit is stored correctly" $ do
                let cfg = defaultFmtConfig{cfgNewlineLimit = 5}
                assertEqual "newline_limit" 5 (cfgNewlineLimit cfg)
            ]
        ]

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) f g x = f x && g x

pragmaTests :: TestTree
pragmaTests =
    testGroup
        "Pragmas"
        [ testCase "Single disabled region" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "//muninn:fmt off"
                        , "foo  ::  1"
                        , "//muninn:fmt on"
                        , ""
                        , "bar :: 2"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "Disabled region preserved verbatim"
                        (T.isInfixOf "foo  ::  1" out)
        , testCase "Multiple disabled regions" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "//muninn:fmt off"
                        , "foo  ::  1"
                        , "//muninn:fmt on"
                        , ""
                        , "normal :: 2"
                        , ""
                        , "//muninn:fmt off"
                        , "bar  ::  3"
                        , "//muninn:fmt on"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "First disabled region preserved"
                        (T.isInfixOf "foo  ::  1" out)
                    assertBool
                        "Second disabled region preserved"
                        (T.isInfixOf "bar  ::  3" out)
        , testCase "Unclosed fmt off" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "//muninn:fmt off"
                        , "foo  ::  1"
                        , "bar  ::  2"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "Rest of file preserved verbatim"
                        (T.isInfixOf "foo  ::  1" out && T.isInfixOf "bar  ::  2" out)
        , testCase "Stray fmt on (ignored)" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "//muninn:fmt on"
                        , "foo  ::  1"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "Stray on ignored, code is formatted"
                        (T.isInfixOf "foo :: 1" out)
        ]

commentTests :: TestTree
commentTests =
    testGroup
        "Comment preservation"
        [ testCase "Comment between declarations" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "foo :: 1"
                        , ""
                        , "// a comment"
                        , "bar :: 2"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "Comment preserved"
                        (T.isInfixOf "// a comment" out)
                    assertBool
                        "Comment before bar"
                        (T.isInfixOf "// a comment\nbar :: 2" out)
        , testCase "Comment inside block" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "foo :: proc() {"
                        , "\t// inside comment"
                        , "\tx := 1"
                        , "}"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "Inner comment preserved"
                        (T.isInfixOf "// inside comment" out)
        , testCase "Doc comment before package" $ do
            let src =
                    T.unlines
                        [ "// Package documentation"
                        , "package test"
                        , ""
                        , "foo :: 1"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "Doc comment preserved"
                        (T.isInfixOf "// Package documentation" out)
                    assertBool
                        "Doc comment before package"
                        (T.isInfixOf "// Package documentation\npackage test" out)
        , testCase "Multiple comments between declarations" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "foo :: 1"
                        , ""
                        , "// first comment"
                        , "// second comment"
                        , "bar :: 2"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "First comment preserved"
                        (T.isInfixOf "// first comment" out)
                    assertBool
                        "Second comment preserved"
                        (T.isInfixOf "// second comment" out)
        , testCase "Comment round-trip" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "// comment about foo"
                        , "foo :: 1"
                        , ""
                        , "bar :: 2"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure $ "Parse original failed: " ++ err
                Right ast1 -> do
                    let formatted = formatOdinWith defaultFmtConfig (Just src) ast1
                    case parseOdin "<formatted>" formatted of
                        Left err -> assertFailure $ "Parse formatted failed: " ++ err ++ "\n\n" ++ T.unpack formatted
                        Right ast2 -> do
                            let json1 = stripForComparison (toJSON (fileToJSON ast1))
                                json2 = stripForComparison (toJSON (fileToJSON ast2))
                            assertEqual "Round-trip AST matches" json1 json2
        , testCase "Fmt off region with comments" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "// normal comment"
                        , "foo :: 1"
                        , ""
                        , "//muninn:fmt off"
                        , "bar  ::  2"
                        , "//muninn:fmt on"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "Normal comment preserved"
                        (T.isInfixOf "// normal comment" out)
                    assertBool
                        "Disabled region preserved verbatim"
                        (T.isInfixOf "bar  ::  2" out)
        , testCase "Inline struct field comments" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "Foo :: struct {"
                        , "\tx: int, // field x"
                        , "\ty: int, // field y"
                        , "}"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        ("Inline comment preserved on x: " ++ T.unpack out)
                        (T.isInfixOf "int, // field x" out)
                    assertBool
                        ("Inline comment preserved on y: " ++ T.unpack out)
                        (T.isInfixOf "int, // field y" out)
        , testCase "Range operator spacing" $ do
            let src =
                    T.unlines
                        [ "package test"
                        , ""
                        , "foo :: proc() {"
                        , "\tfor i in 0..<10 {}"
                        , "\tfor j in 0..=10 {}"
                        , "}"
                        ]
            case parseOdin "<test>" src of
                Left err -> assertFailure err
                Right ast -> do
                    let out = formatOdinWith defaultFmtConfig (Just src) ast
                    assertBool
                        "Range half has spaces"
                        (T.isInfixOf "0 ..< 10" out)
                    assertBool
                        "Range full has spaces"
                        (T.isInfixOf "0 ..= 10" out)
        ]

configFileTests :: TestTree
configFileTests =
    testGroup
        "Config file parsing"
        [ testCase "Full config" $ do
            let toml =
                    T.unlines
                        [ "[fmt]"
                        , "indent_style = \"spaces\""
                        , "indent_width = 2"
                        , "line_width = 80"
                        , "newline_limit = 3"
                        ]
            case parseConfigToml toml of
                Left err -> assertFailure err
                Right cfg -> do
                    assertEqual "indent_style" Spaces (cfgIndentStyle cfg)
                    assertEqual "indent_width" 2 (cfgIndentWidth cfg)
                    assertEqual "line_width" 80 (cfgLineWidth cfg)
                    assertEqual "newline_limit" 3 (cfgNewlineLimit cfg)
        , testCase "Partial config" $ do
            let toml =
                    T.unlines
                        [ "[fmt]"
                        , "indent_width = 8"
                        ]
            case parseConfigToml toml of
                Left err -> assertFailure err
                Right cfg -> do
                    assertEqual "indent_width overridden" 8 (cfgIndentWidth cfg)
                    assertEqual "indent_style default" Tabs (cfgIndentStyle cfg)
                    assertEqual "line_width default" 100 (cfgLineWidth cfg)
                    assertEqual "newline_limit default" 2 (cfgNewlineLimit cfg)
        , testCase "Empty / no [fmt] section" $ do
            let toml = "# just a comment\n"
            case parseConfigToml toml of
                Left err -> assertFailure err
                Right cfg ->
                    assertEqual "defaults" defaultFmtConfig cfg
        , testCase "Invalid values" $ do
            let toml =
                    T.unlines
                        [ "[fmt]"
                        , "indent_width = abc"
                        ]
            case parseConfigToml toml of
                Left _ -> pure ()
                Right _ -> assertFailure "Expected parse error for invalid value"
        ]

cliTests :: FilePath -> TestTree
cliTests exe =
    testGroup
        "CLI flags"
        [ testCase "--print-config (no config file)" $ do
            withSystemTempDirectory "muninn-test" $ \tmpDir -> do
                (code, out, _err) <-
                    readProcessWithExitCode
                        exe
                        ["fmt", "--print-config", tmpDir]
                        ""
                assertEqual "exit code" ExitSuccess code
                assertBool "contains indent_style" ("indent_style" `elem` concatMap words (lines out))
                assertBool "contains tabs default" (any (== "\"tabs\"") (words out))
        , testCase "--print-config (with muninn.toml)" $ do
            withSystemTempDirectory "muninn-test" $ \tmpDir -> do
                writeFile (tmpDir </> "muninn.toml") $
                    unlines
                        [ "[fmt]"
                        , "indent_style = \"spaces\""
                        , "indent_width = 2"
                        ]
                (code, out, _err) <-
                    readProcessWithExitCode
                        exe
                        ["fmt", "--print-config", tmpDir]
                        ""
                assertEqual "exit code" ExitSuccess code
                assertBool "contains spaces" (any (== "\"spaces\"") (words out))
        , testCase "--stdin" $ do
            let src = "package test\n\nfoo  ::  1\n"
            (code, out, _err) <-
                readProcessWithExitCode
                    exe
                    ["fmt", "--stdin"]
                    src
            assertEqual "exit code" ExitSuccess code
            assertBool "output is formatted" ("foo :: 1" `isInfixOf` out)
        , testCase "--check (no changes)" $ do
            let src = "package test\n\nfoo :: 1\n"
            (code, _out, _err) <-
                readProcessWithExitCode
                    exe
                    ["fmt", "--stdin", "--check"]
                    src
            assertEqual "exit code 0 for already formatted" ExitSuccess code
        , testCase "--check (has changes)" $ do
            let src = "package test\n\nfoo  ::  1\n"
            (code, _out, _err) <-
                readProcessWithExitCode
                    exe
                    ["fmt", "--stdin", "--check"]
                    src
            assertEqual "exit code 1 for needs formatting" (ExitFailure 1) code
        , testCase "--diff" $ do
            withSystemTempDirectory "muninn-test" $ \tmpDir -> do
                let fp = tmpDir </> "test.odin"
                writeFile fp "package test\n\nfoo  ::  1\n"
                (code, out, _err) <-
                    readProcessWithExitCode
                        exe
                        ["fmt", "--diff", fp]
                        ""
                assertEqual "exit code" ExitSuccess code
                assertBool "contains ---" ("---" `isInfixOf` out)
                assertBool "contains +++" ("+++" `isInfixOf` out)
        , testCase "--diff --output-json" $ do
            withSystemTempDirectory "muninn-test" $ \tmpDir -> do
                let fp = tmpDir </> "test.odin"
                writeFile fp "package test\n\nfoo  ::  1\n"
                (code, out, _err) <-
                    readProcessWithExitCode
                        exe
                        ["fmt", "--diff", "--output-json", fp]
                        ""
                assertEqual "exit code" ExitSuccess code
                assertBool "contains file key" ("\"file\"" `isInfixOf` out)
                assertBool "contains diff key" ("\"diff\"" `isInfixOf` out)
        ]

stripForComparison :: Value -> Value
stripForComparison = stripForeignPaths . flattenBlocks . stripPositions

stripPositions :: Value -> Value
stripPositions = \case
    Object obj -> Object $ KM.map stripPositions $ KM.filterWithKey (\k _ -> k `notElem` posKeys) obj
    Array arr -> Array $ fmap stripPositions arr
    v -> v
  where
    posKeys = [Key.fromString "pos", Key.fromString "end"]

stripForeignPaths :: Value -> Value
stripForeignPaths = \case
    Object obj
        | Just (String "ForeignImportDecl") <- KM.lookup (Key.fromString "node") obj
        , Just (Array paths) <- KM.lookup (Key.fromString "paths") obj
        , paths == V.singleton (String "") ->
            Object $ KM.map stripForeignPaths $ KM.insert (Key.fromString "paths") (Array V.empty) obj
        | otherwise -> Object $ KM.map stripForeignPaths obj
    Array arr -> Array $ fmap stripForeignPaths arr
    v -> v

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
        , Just (Array inner) <- KM.lookup (Key.fromString "stmts") obj =
            inner
    expandBlock v = V.singleton v

findFirstDiff :: Value -> Value -> String
findFirstDiff (Object o1) (Object o2)
    | Just (Array d1) <- KM.lookup (Key.fromString "decls") o1
    , Just (Array d2) <- KM.lookup (Key.fromString "decls") o2 =
        let pairs = zip3 [0 :: Int ..] (V.toList d1) (V.toList d2)
            diffs = [(i, a, b) | (i, a, b) <- pairs, a /= b]
         in case diffs of
                []
                    | V.length d1 /= V.length d2 ->
                        "Decl count: " ++ show (V.length d1) ++ " vs " ++ show (V.length d2)
                    | otherwise ->
                        let c1 = KM.lookup (Key.fromString "comments") o1
                            c2 = KM.lookup (Key.fromString "comments") o2
                         in if c1 /= c2
                                then
                                    let countGroups (Just (Array arr)) = V.length arr
                                        countGroups _ = 0
                                        ng1 = countGroups c1
                                        ng2 = countGroups c2
                                        firstDiffGroup = case (c1, c2) of
                                            (Just (Array a1), Just (Array a2)) ->
                                                let ps = zip3 [0 :: Int ..] (V.toList a1) (V.toList a2)
                                                    ds = [(i, x, y) | (i, x, y) <- ps, x /= y]
                                                 in case ds of
                                                        ((i, x, y) : _) ->
                                                            "\n  First diff at group "
                                                                ++ show i
                                                                ++ ":\n  ORIG: "
                                                                ++ take 500 (BL.unpack (encodePretty x))
                                                                ++ "\n  FMT:  "
                                                                ++ take 500 (BL.unpack (encodePretty y))
                                                        [] ->
                                                            if ng1 /= ng2
                                                                then "\n  Same prefix but different count"
                                                                else ""
                                            _ -> ""
                                     in "Comments differ: " ++ show ng1 ++ " vs " ++ show ng2 ++ " groups" ++ firstDiffGroup
                                else "Top-level match but pkg/node differ"
                ((i, a, b) : _) ->
                    "First diff at decl "
                        ++ show i
                        ++ ":\n"
                        ++ "  ORIG: "
                        ++ take 1500 (BL.unpack (encodePretty a))
                        ++ "\n"
                        ++ "  FMT:  "
                        ++ take 1500 (BL.unpack (encodePretty b))
findFirstDiff a b = "Structural: " ++ take 200 (show a) ++ " vs " ++ take 200 (show b)

findOdinFiles :: FilePath -> IO [FilePath]
findOdinFiles dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure []
        else do
            entries <- listDirectory dir
            let files = filter (\f -> takeExtension f == ".odin") entries
                filePaths = map (dir </>) files
            subdirs <- filterM doesDirectoryExist (map (dir </>) entries)
            subFiles <- concat <$> mapM findOdinFiles subdirs
            pure $ filePaths ++ subFiles
