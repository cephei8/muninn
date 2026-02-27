module Muninn.Formatter (
    formatOdin,
    formatOdinWith,
    formatFile,
    formatFileWith,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Muninn.Formatter.Config (FmtConfig (..), defaultFmtConfig)
import Muninn.Formatter.Print
import Muninn.Formatter.Stmt (fmtStmt)
import Muninn.Parser (parseFile)
import Muninn.Parser.AST
import Muninn.Parser.SrcLoc (SrcPos (..), SrcSpan (..))

formatOdin :: File SrcSpan -> Text
formatOdin = formatOdinWith defaultFmtConfig Nothing

formatOdinWith :: FmtConfig -> Maybe Text -> File SrcSpan -> Text
formatOdinWith cfg mSrc file = case mSrc of
    Nothing -> runPrinter cfg (fileComments file) (fmtFile file)
    Just src -> runPrinter cfg (fileComments file) (fmtFileWithPragmas cfg src file)

formatFile :: FilePath -> IO (Either String Text)
formatFile = formatFileWith defaultFmtConfig

formatFileWith :: FmtConfig -> FilePath -> IO (Either String Text)
formatFileWith cfg path = do
    src <- TIO.readFile path
    result <- parseFile path
    case result of
        Left err -> pure (Left err)
        Right ast -> pure (Right (formatOdinWith cfg (Just src) ast))

fmtFile :: File SrcSpan -> Printer ()
fmtFile (File _sp pkg decls _comments docs) = do
    emitDocComment docs
    emit "package "
    emit pkg
    case decls of
        [] -> do
            newline
            drainRemainingComments
        _ -> do
            newline
            fmtDeclList decls
            drainRemainingComments
            emit "\n"

emitDocComment :: Maybe CommentGroup -> Printer ()
emitDocComment Nothing = pure ()
emitDocComment (Just docCg) = do
    mapM_ (\(Comment _ text) -> emit text >> emit "\n") (cgList docCg)
    skipCommentsBefore (posOffset (spanEnd (cgSpan docCg)) + 1)
    setLastLine (posLine (spanEnd (cgSpan docCg)))

fmtDeclList :: [Stmt SrcSpan] -> Printer ()
fmtDeclList decls = mapM_ fmtOneDecl decls
  where
    fmtOneDecl d = do
        let off = posOffset (spanStart (stmtSpan d))
            ln = posLine (spanStart (stmtSpan d))
        drainCommentsBefore off
        emitBlankLineSep ln
        newline
        fmtStmt d
        drainLineCommentAfter (posLine (spanEnd (stmtSpan d)))
        setLastLine (posLine (spanEnd (stmtSpan d)))

fmtFileWithPragmas :: FmtConfig -> Text -> File SrcSpan -> Printer ()
fmtFileWithPragmas _cfg src (File _sp pkg decls comments docs) = do
    let disabled = findDisabledRanges comments
    emitDocComment docs
    emit "package "
    emit pkg
    case decls of
        [] -> do
            newline
            drainRemainingComments
        _ -> do
            newline
            fmtDeclListWithPragmas disabled src decls
            drainRemainingComments
            emit "\n"

fmtDeclListWithPragmas :: [(Int, Int)] -> Text -> [Stmt SrcSpan] -> Printer ()
fmtDeclListWithPragmas _ _ [] = pure ()
fmtDeclListWithPragmas disabled src (d : ds) = do
    fmtOne d
    mapM_ fmtOne ds
  where
    fmtOne decl = do
        let sp = stmtSpan decl
            off = posOffset (spanStart sp)
            ln = posLine (spanStart sp)
        drainCommentsBefore off
        emitBlankLineSep ln
        newline
        fmtOneDeclWithPragma disabled src decl
        drainLineCommentAfter (posLine (spanEnd sp))
        setLastLine (posLine (spanEnd sp))

fmtOneDeclWithPragma :: [(Int, Int)] -> Text -> Stmt SrcSpan -> Printer ()
fmtOneDeclWithPragma disabled src decl = do
    let sp = stmtSpan decl
        startLine = posLine (spanStart sp)
    if any (\(lo, hi) -> startLine >= lo && startLine <= hi) disabled
        then do
            let startOff = posOffset (spanStart sp)
                endOff = posOffset (spanEnd sp)
                verbatim = textSlice startOff endOff src
            emit verbatim
        else
            fmtStmt decl

textSlice :: Int -> Int -> Text -> Text
textSlice start end t = T.take (end - start) (T.drop start t)

findDisabledRanges :: [CommentGroup] -> [(Int, Int)]
findDisabledRanges cgs = go False 0 allComments
  where
    allComments = concatMap cgList cgs

    go :: Bool -> Int -> [Comment] -> [(Int, Int)]
    go False _ [] = []
    go True offStart [] = [(offStart, maxBound)]
    go False _ (c : cs)
        | isFmtOff (commentText c) = go True (posLine (commentPos c)) cs
        | otherwise = go False 0 cs
    go True offStart (c : cs)
        | isFmtOn (commentText c) = (offStart, posLine (commentPos c)) : go False 0 cs
        | otherwise = go True offStart cs

    isFmtOff t =
        T.strip (T.stripPrefix "//" (T.strip t) `orEmpty` t) == "muninn:fmt off"
            || T.strip t == "//muninn:fmt off"
    isFmtOn t =
        T.strip (T.stripPrefix "//" (T.strip t) `orEmpty` t) == "muninn:fmt on"
            || T.strip t == "//muninn:fmt on"

    orEmpty Nothing x = x
    orEmpty (Just v) _ = v
