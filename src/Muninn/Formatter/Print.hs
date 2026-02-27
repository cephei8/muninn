module Muninn.Formatter.Print (
    Printer,
    runPrinter,
    emit,
    newline,
    space,
    withIndent,
    emitIndent,
    bracesBlock,
    commaSep,
    parens,
    brackets,
    sepBy,
    getConfig,
    drainCommentsBefore,
    setLastLine,
    getLastLine,
    emitBlankLineSep,
    drainRemainingComments,
    drainLineCommentAfter,
    hasCommentsBefore,
    skipCommentsBefore,
) where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State.Strict (State, execState, gets, modify')
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as B

import Muninn.Formatter.Config (FmtConfig (..), IndentStyle (..))
import Muninn.Parser.AST (Comment (..), CommentGroup (..))
import Muninn.Parser.SrcLoc (SrcPos (..), SrcSpan (..))

data PrintEnv = PrintEnv
    { peDepth :: !Int
    , peConfig :: !FmtConfig
    }

data PrintState = PrintState
    { psBuilder :: !Builder
    , psComments :: [CommentGroup]
    , psLastLine :: !Int
    , psAfterInlineComment :: !Bool
    }

type Printer a = ReaderT PrintEnv (State PrintState) a

runPrinter :: FmtConfig -> [CommentGroup] -> Printer () -> Text
runPrinter cfg comments p =
    TL.toStrict $ B.toLazyText $ psBuilder $ execState (runReaderT p env0) st0
  where
    env0 = PrintEnv{peDepth = 0, peConfig = cfg}
    st0 = PrintState{psBuilder = mempty, psComments = comments, psLastLine = 0, psAfterInlineComment = False}

emit :: Text -> Printer ()
emit t = modify' (\s -> s{psBuilder = psBuilder s <> B.fromText t})

emitIndent :: Printer ()
emitIndent = do
    depth <- asks peDepth
    style <- asks (cfgIndentStyle . peConfig)
    width <- asks (cfgIndentWidth . peConfig)
    case style of
        Tabs -> emit (T.replicate depth "\t")
        Spaces -> emit (T.replicate (depth * width) " ")

newline :: Printer ()
newline = do
    emit "\n"
    emitIndent

space :: Printer ()
space = emit " "

withIndent :: Printer a -> Printer a
withIndent = local (\env -> env{peDepth = peDepth env + 1})

getConfig :: Printer FmtConfig
getConfig = asks peConfig

bracesBlock :: Printer () -> Printer ()
bracesBlock body = do
    emit " {"
    setLastLine 0
    withIndent body
    newline
    emit "}"

parens :: Printer () -> Printer ()
parens body = do
    emit "("
    body
    emit ")"

brackets :: Printer () -> Printer ()
brackets body = do
    emit "["
    body
    emit "]"

commaSep :: [Printer ()] -> Printer ()
commaSep = sepBy (emit ", ")

sepBy :: Printer () -> [Printer ()] -> Printer ()
sepBy _ [] = pure ()
sepBy _ [x] = x
sepBy sep (x : xs) = do
    x
    mapM_ (\a -> sep >> a) xs

getLastLine :: Printer Int
getLastLine = gets psLastLine

setLastLine :: Int -> Printer ()
setLastLine ln = modify' (\s -> s{psLastLine = ln})

emitBlankLineSep :: Int -> Printer ()
emitBlankLineSep targetLine = do
    modify' (\s -> s{psAfterInlineComment = False})
    lastLn <- getLastLine
    when (lastLn > 0 && targetLine > lastLn + 1) $
        emit "\n"

drainCommentsBefore :: Int -> Printer ()
drainCommentsBefore offset = do
    cgs <- gets psComments
    let (before, after) = span (\cg -> posOffset (spanStart (cgSpan cg)) < offset) cgs
    modify' (\s -> s{psComments = after})
    emitCommentGroupsSep before

drainLineCommentAfter :: Int -> Printer ()
drainLineCommentAfter srcLine = do
    cgs <- gets psComments
    case cgs of
        (cg : rest)
            | posLine (spanStart (cgSpan cg)) == srcLine
            , posLine (spanEnd (cgSpan cg)) == srcLine -> do
                modify' (\s -> s{psComments = rest, psAfterInlineComment = True})
                mapM_ (\(Comment _ text) -> emit " " >> emit text) (cgList cg)
        _ -> pure ()

drainRemainingComments :: Printer ()
drainRemainingComments = do
    cgs <- gets psComments
    modify' (\s -> s{psComments = []})
    emitCommentGroupsSep cgs

hasCommentsBefore :: Int -> Printer Bool
hasCommentsBefore offset = do
    cgs <- gets psComments
    case cgs of
        (cg : _) | posOffset (spanStart (cgSpan cg)) < offset -> pure True
        _ -> pure False

skipCommentsBefore :: Int -> Printer ()
skipCommentsBefore offset =
    modify' (\s -> s{psComments = dropWhile (\cg -> posOffset (spanStart (cgSpan cg)) < offset) (psComments s)})

emitCommentGroupsSep :: [CommentGroup] -> Printer ()
emitCommentGroupsSep [] = pure ()
emitCommentGroupsSep [cg] = emitCommentGroup cg
emitCommentGroupsSep (cg : rest) = do
    emitCommentGroup cg
    mapM_ emitGroupFixedSep rest
  where
    emitGroupFixedSep (CommentGroup cgsp comments) = do
        emit "\n\n"
        mapM_ (\(Comment _pos text) -> newline >> emit text) comments
        setLastLine (posLine (spanEnd cgsp))

emitCommentGroup :: CommentGroup -> Printer ()
emitCommentGroup (CommentGroup cgsp comments) = do
    afterInline <- gets psAfterInlineComment
    if afterInline
        then do
            emit "\n\n"
            modify' (\s -> s{psAfterInlineComment = False})
        else do
            let cgLine = posLine (spanStart cgsp)
            emitBlankLineSep cgLine
    mapM_ emitOneComment comments
    setLastLine (posLine (spanEnd cgsp))
  where
    emitOneComment (Comment _pos text) = do
        newline
        emit text
