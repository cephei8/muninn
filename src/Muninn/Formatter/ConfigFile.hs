module Muninn.Formatter.ConfigFile (
    findConfig,
    loadConfig,
    parseConfigToml,
    showConfig,
) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

import Muninn.Formatter.Config (FmtConfig (..), IndentStyle (..), defaultFmtConfig)

findConfig :: FilePath -> IO (Maybe FilePath)
findConfig dir = do
    let candidate = dir </> "muninn.toml"
    exists <- doesFileExist candidate
    if exists
        then pure (Just candidate)
        else do
            let parent = takeDirectory dir
            if parent == dir
                then pure Nothing
                else findConfig parent

loadConfig :: FilePath -> IO FmtConfig
loadConfig dir = do
    mPath <- findConfig dir
    case mPath of
        Nothing -> pure defaultFmtConfig
        Just path -> do
            contents <- TIO.readFile path
            case parseConfigToml contents of
                Left _err -> pure defaultFmtConfig
                Right cfg -> pure cfg

parseConfigToml :: Text -> Either String FmtConfig
parseConfigToml input = applyPairs defaultFmtConfig pairs
  where
    ls = map T.strip (T.lines input)
    inFmt = takeWhile (not . isSectionHeader) $ drop 1 $ dropWhile (/= "[fmt]") ls
    pairs = concatMap parseLine inFmt

    isSectionHeader t = T.isPrefixOf "[" t && T.isSuffixOf "]" t && t /= "[fmt]"

    parseLine line
        | T.null line = []
        | T.isPrefixOf "#" line = []
        | otherwise = case T.breakOn "=" line of
            (_, rest) | T.null rest -> []
            (k, v) -> [(T.strip k, T.strip (T.drop 1 v))]

    applyPairs cfg [] = Right cfg
    applyPairs cfg ((k, v) : rest) = case applyPair cfg k v of
        Left err -> Left err
        Right cfg' -> applyPairs cfg' rest

    applyPair cfg "line_width" v = case readInt v of
        Just n -> Right cfg{cfgLineWidth = n}
        Nothing -> Left $ "invalid line_width: " ++ T.unpack v
    applyPair cfg "indent_width" v = case readInt v of
        Just n -> Right cfg{cfgIndentWidth = n}
        Nothing -> Left $ "invalid indent_width: " ++ T.unpack v
    applyPair cfg "indent_style" v = case parseStyle v of
        Just s -> Right cfg{cfgIndentStyle = s}
        Nothing -> Left $ "invalid indent_style: " ++ T.unpack v
    applyPair cfg _ _ = Right cfg

    readInt t = case reads (T.unpack (stripQuotes t)) of
        [(n, rest)] | all isSpace rest -> Just n
        _ -> Nothing

    stripQuotes t
        | T.isPrefixOf "\"" t && T.isSuffixOf "\"" t = T.drop 1 (T.dropEnd 1 t)
        | otherwise = t

    parseStyle t =
        let t' = T.toLower (stripQuotes t)
         in case t' of
                "tabs" -> Just Tabs
                "spaces" -> Just Spaces
                _ -> Nothing

showConfig :: FmtConfig -> Text
showConfig cfg =
    T.unlines
        [ "[fmt]"
        , "indent_style = \"" <> styleText <> "\""
        , "indent_width = " <> T.pack (show (cfgIndentWidth cfg))
        , "line_width = " <> T.pack (show (cfgLineWidth cfg))
        ]
  where
    styleText = case cfgIndentStyle cfg of
        Tabs -> "tabs"
        Spaces -> "spaces"
