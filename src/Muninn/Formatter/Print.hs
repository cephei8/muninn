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
    getColumn,
    tryInline,
    drainCommentsBefore,
    setLastLine,
    getLastLine,
    emitBlankLineSep,
    drainRemainingComments,
    drainLineCommentAfter,
    hasCommentsBefore,
    skipCommentsBefore,
    popCommentsBefore,
) where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State.Strict (State, execState, get, gets, modify', put)
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
    , peForceInline :: !Bool
    }

data PrintState = PrintState
    { psBuilder :: !Builder
    , psComments :: [CommentGroup]
    , psLastLine :: !Int
    , psAfterInlineComment :: !Bool
    , psColumn :: !Int
    , psNewlineCount :: !Int
    }

type Printer a = ReaderT PrintEnv (State PrintState) a

runPrinter :: FmtConfig -> [CommentGroup] -> Printer () -> Text
runPrinter cfg comments p =
    TL.toStrict $ B.toLazyText $ psBuilder $ execState (runReaderT p env0) st0
  where
    env0 = PrintEnv{peDepth = 0, peConfig = cfg, peForceInline = False}
    st0 = PrintState{psBuilder = mempty, psComments = comments, psLastLine = 0, psAfterInlineComment = False, psColumn = 0, psNewlineCount = 0}

emit :: Text -> Printer ()
emit t = modify' (\s -> s{psBuilder = psBuilder s <> B.fromText t, psColumn = psColumn s + T.length t})

emitIndent :: Printer ()
emitIndent = do
    depth <- asks peDepth
    style <- asks (cfgIndentStyle . peConfig)
    width <- asks (cfgIndentWidth . peConfig)
    let (txt, col) = case style of
            Tabs -> (T.replicate depth "\t", depth * width)
            Spaces -> let n = depth * width in (T.replicate n " ", n)
    modify' (\s -> s{psBuilder = psBuilder s <> B.fromText txt, psColumn = col})

newline :: Printer ()
newline = do
    modify' (\s -> s{psBuilder = psBuilder s <> B.singleton '\n', psColumn = 0, psNewlineCount = psNewlineCount s + 1, psAfterInlineComment = False})
    emitIndent

space :: Printer ()
space = emit " "

withIndent :: Printer a -> Printer a
withIndent = local (\env -> env{peDepth = peDepth env + 1})

getConfig :: Printer FmtConfig
getConfig = asks peConfig

getColumn :: Printer Int
getColumn = gets psColumn

tryInline :: Printer () -> Printer () -> Printer ()
tryInline inline multiLine = do
    forceInline <- asks peForceInline
    if forceInline
        then inline
        else do
            cfg <- getConfig
            st <- get
            local (\e -> e{peForceInline = True}) inline
            col <- getColumn
            nlCount <- gets psNewlineCount
            if col > cfgLineWidth cfg || nlCount > psNewlineCount st
                then put st >> multiLine
                else pure ()

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

-- | Drain a single-line comment group that sits on @srcLine@ and whose start
-- offset is strictly less than @maxOffset@.  The @maxOffset@ bound prevents a
-- comment from being claimed by an earlier item when multiple items share the
-- same source line (e.g. all proc params written on one line).
drainLineCommentAfter :: Int -> Int -> Printer ()
drainLineCommentAfter srcLine maxOffset = do
    cgs <- gets psComments
    case cgs of
        (cg : rest)
            | posLine (spanStart (cgSpan cg)) == srcLine
            , posLine (spanEnd (cgSpan cg)) == srcLine
            , posOffset (spanStart (cgSpan cg)) < maxOffset -> do
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

-- | Remove and return all comment groups whose start offset is strictly less
-- than @offset@, without emitting them.
popCommentsBefore :: Int -> Printer [CommentGroup]
popCommentsBefore offset = do
    cgs <- gets psComments
    let (before, after) = span (\cg -> posOffset (spanStart (cgSpan cg)) < offset) cgs
    modify' (\s -> s{psComments = after})
    pure before

emitCommentList :: [Comment] -> Printer ()
emitCommentList [] = pure ()
emitCommentList (c : cs) = do
    emitFirst c
    mapM_ (uncurry emitNext) (zip (c : cs) cs)
  where
    emitFirst (Comment _pos text) = newline >> emit text
    emitNext (Comment prevPos prevText) (Comment pos text) = do
        let prevEndLine = posLine prevPos + T.count "\n" prevText
        when (posLine pos > prevEndLine + 1) $ emit "\n"
        newline
        emit text

emitCommentGroupsSep :: [CommentGroup] -> Printer ()
emitCommentGroupsSep [] = pure ()
emitCommentGroupsSep [cg] = emitCommentGroup cg
emitCommentGroupsSep (cg : rest) = do
    emitCommentGroup cg
    mapM_ emitGroupFixedSep rest
  where
    emitGroupFixedSep (CommentGroup cgsp comments) = do
        -- Emit 2 blank lines between separate comment groups so that
        -- re-parsing keeps them in separate groups (≥2 blank lines required).
        emit "\n\n"
        emitCommentList comments
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
    emitCommentList comments
    setLastLine (posLine (spanEnd cgsp))
