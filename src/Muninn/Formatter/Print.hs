module Muninn.Formatter.Print (
    Printer,
    runPrinter,
    emit,
    newline,
    space,
    withIndent,
    inCallArgs,
    withBinChainIndent,
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
    , peInCallArgs :: !Bool
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
    env0 = PrintEnv{peDepth = 0, peConfig = cfg, peForceInline = False, peInCallArgs = False}
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

-- | Mark that we are formatting direct call arguments.  Binary chains inside
-- call arguments should not add an extra indentation level on top of the
-- argument indentation already established by 'withIndent' in the call site.
inCallArgs :: Printer a -> Printer a
inCallArgs = local (\env -> env{peInCallArgs = True})

-- | Indent a binary-chain continuation, unless we are directly inside call
-- arguments, in which case the call's own 'withIndent' already provides the
-- right indentation level.  Either way, the 'peInCallArgs' flag is cleared so
-- that nested binary chains (e.g. in conditions) behave normally.
withBinChainIndent :: Printer a -> Printer a
withBinChainIndent body = do
    inArgs <- asks peInCallArgs
    let cleared = local (\env -> env{peInCallArgs = False}) body
    if inArgs then cleared else withIndent cleared

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

-- | Drain comments that sit on @srcLine@ from the head of the comment queue,
-- whose group start offset is strictly less than @maxOffset@.  When the head
-- group spans multiple source lines (e.g. an inline comment on @srcLine@
-- grouped with a standalone comment on @srcLine+1@), only the comments that
-- actually fall on @srcLine@ are emitted; the remaining comments are kept in
-- the queue as a new (partial) group.
drainLineCommentAfter :: Int -> Int -> Printer ()
drainLineCommentAfter srcLine maxOffset = do
    cgs <- gets psComments
    case cgs of
        (CommentGroup cgsp comments : rest)
            | posLine (spanStart cgsp) == srcLine
            , posOffset (spanStart cgsp) < maxOffset ->
                let (onLine, offLine) = span (\c -> posLine (commentPos c) == srcLine) comments
                 in case onLine of
                        [] -> pure ()
                        _ -> do
                            let (newCgs, afterInline) = case offLine of
                                    [] -> (rest, True)
                                    (c : _) ->
                                        ( CommentGroup (SrcSpan (commentPos c) (spanEnd cgsp)) offLine : rest
                                        , False
                                        )
                            modify' (\s -> s{psComments = newCgs, psAfterInlineComment = afterInline})
                            mapM_ (\(Comment _ text) -> emit " " >> emit text) onLine
                            let Comment lastPos lastText = last onLine
                                commentEndLine = posLine lastPos + T.count "\n" lastText
                            setLastLine commentEndLine
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
        if posLine pos == prevEndLine
            then emit text -- same source line: no newline between comments
            else do
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
    modify' (\s -> s{psAfterInlineComment = False})
    if afterInline
        then emit "\n"
        else do
            let cgLine = posLine (spanStart cgsp)
            emitBlankLineSep cgLine
    emitCommentList comments
    setLastLine (posLine (spanEnd cgsp))
