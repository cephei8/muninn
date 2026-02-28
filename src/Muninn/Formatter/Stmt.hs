module Muninn.Formatter.Stmt (
    fmtStmt,
    fmtExpr,
    stmtLeadingStart,
) where

import Muninn.Formatter.Expr (fmtExprWith)
import Muninn.Formatter.Print
import Muninn.Parser.AST
import Muninn.Parser.JSON (showAssignOp, showBranchKind)
import Muninn.Parser.SrcLoc (SrcPos (..), SrcSpan (..))

fmtExpr :: Expr SrcSpan -> Printer ()
fmtExpr = fmtExprWith fmtStmt

fmtStmt :: Stmt SrcSpan -> Printer ()
fmtStmt = \case
    PackageDecl _sp name -> do
        emit "package "
        emit name
    ImportDecl _sp attrs mAlias path -> do
        mapM_ (\a -> fmtAttribute a >> newline) (reverse attrs)
        emit "import "
        case mAlias of
            Just alias -> do
                emit alias
                space
            Nothing -> pure ()
        emit "\""
        emit path
        emit "\""
    ForeignImportDecl _sp attrs name paths -> do
        mapM_ (\a -> fmtAttribute a >> newline) (reverse attrs)
        emit "foreign import "
        emit name
        case paths of
            [p] -> do
                space
                emit "\""
                emit p
                emit "\""
            [] -> do space; emit "\"\""
            _ -> bracesBlock (mapM_ (\p -> newline >> emit "\"" >> emit p >> emit "\",") paths)
    ForeignBlockDecl _sp attrs mLib body -> do
        mapM_ (\a -> fmtAttribute a >> newline) (reverse attrs)
        emit "foreign"
        case mLib of
            Just lib -> do
                space
                fmtExpr lib
            Nothing -> pure ()
        space
        fmtBlockBody body
    ValueDecl _sp attrs names mTy vals isMut -> do
        mapM_ (\a -> fmtAttribute a >> newline) (reverse attrs)
        commaSep (map fmtExpr names)
        case (mTy, vals, isMut) of
            (Nothing, _ : _, False) -> do
                emit " :: "
                commaSep (map fmtExpr vals)
            (Nothing, _ : _, True) -> do
                emit " := "
                commaSep (map fmtExpr vals)
            (Just ty, _ : _, False) -> do
                emit " : "
                fmtExpr ty
                emit " : "
                commaSep (map fmtExpr vals)
            (Just ty, _ : _, True) -> do
                emit " : "
                fmtExpr ty
                emit " = "
                commaSep (map fmtExpr vals)
            (Just ty, [], _) -> do
                emit " : "
                fmtExpr ty
            (Nothing, [], _) -> pure ()
    ExprStmt _sp e -> fmtExpr e
    AssignStmt _sp lhs op rhs -> do
        commaSep (map fmtExpr lhs)
        space
        emit (showAssignOp op)
        space
        commaSep (map fmtExpr rhs)
    BlockStmt _sp mLabel stmts _isDo -> case mLabel of
        Just label -> do
            fmtExpr label
            emit ": "
            emit "{"
            withIndent $ fmtStmtList stmts
            newline
            emit "}"
        Nothing -> case stmts of
            [] -> pure ()
            _ -> do
                emit "{"
                withIndent $ fmtStmtList stmts
                newline
                emit "}"
    IfStmt _sp mLabel mInit cond body mElse -> do
        case mLabel of
            Just label -> do fmtExpr label; emit ": "
            Nothing -> pure ()
        emit "if "
        case mInit of
            Just ini -> do
                fmtStmt ini
                emit "; "
            Nothing -> pure ()
        fmtExpr cond
        space
        fmtBlockBody body
        case mElse of
            Just els -> do
                emit " else "
                fmtBlockBody els
            Nothing -> pure ()
    WhenStmt _sp cond body mElse -> do
        emit "when "
        fmtExpr cond
        space
        fmtBlockBody body
        case mElse of
            Just els -> do
                emit " else "
                fmtBlockBody els
            Nothing -> pure ()
    ForStmt _sp mLabel mInit mCond mPost body -> do
        case mLabel of
            Just label -> do fmtExpr label; emit ": "
            Nothing -> pure ()
        emit "for "
        case (mInit, mCond, mPost) of
            (Nothing, Nothing, Nothing) -> pure ()
            (Nothing, Just cond, Nothing) -> do
                fmtExpr cond
                space
            _ -> do
                maybe (pure ()) fmtStmt mInit
                emit "; "
                maybe (pure ()) fmtExpr mCond
                emit "; "
                maybe (pure ()) fmtStmt mPost
                space
        fmtBlockBody body
    RangeStmt _sp mLabel vals range body rev -> do
        case mLabel of
            Just label -> do fmtExpr label; emit ": "
            Nothing -> pure ()
        if rev then emit "#reverse " else pure ()
        emit "for "
        case vals of
            [] -> emit "in "
            _ -> do
                commaSep (map fmtExpr vals)
                emit " in "
        fmtExpr range
        space
        fmtBlockBody body
    SwitchStmt _sp mLabel mInit mTag body partial -> do
        case mLabel of
            Just label -> do fmtExpr label; emit ": "
            Nothing -> pure ()
        if partial then emit "#partial " else pure ()
        emit "switch "
        case mInit of
            Just ini -> do
                fmtStmt ini
                emit "; "
            Nothing -> pure ()
        case mTag of
            Just tag -> do
                fmtExpr tag
                space
            Nothing -> pure ()
        fmtSwitchBody body
    TypeSwitchStmt _sp mLabel mInit mTag body partial -> do
        case mLabel of
            Just label -> do fmtExpr label; emit ": "
            Nothing -> pure ()
        if partial then emit "#partial " else pure ()
        emit "switch "
        case mInit of
            Just ini -> do
                fmtTypeSwitchInit ini
                case mTag of
                    Just _ -> emit "; "
                    Nothing -> space
            Nothing -> pure ()
        case mTag of
            Just tag -> fmtExpr tag >> space
            Nothing -> pure ()
        fmtSwitchBody body
    CaseClause _sp exprs stmts -> do
        if null exprs
            then emit "case"
            else do emit "case "; commaSep (map fmtExpr exprs)
        emit ":"
        withIndent $ fmtStmtList stmts
    ReturnStmt _sp vals -> do
        emit "return"
        case vals of
            [] -> pure ()
            _ -> do
                space
                commaSep (map fmtExpr vals)
    DeferStmt _sp s -> do
        emit "defer "
        fmtBlockBody s
    BranchStmt _sp kind mLabel -> do
        emit (showBranchKind kind)
        case mLabel of
            Just label -> do
                space
                fmtExpr label
            Nothing -> pure ()
    UsingStmt _sp exprs -> do
        emit "using "
        commaSep (map fmtExpr exprs)
    DirectiveStmt _sp name inner -> do
        emit "#"
        emit name
        space
        fmtBlockBody inner
    BadStmt _sp -> emit "/* BAD STMT */"

fmtStmtList :: [Stmt SrcSpan] -> Printer ()
fmtStmtList stmts = do
    setLastLine 0
    mapM_ fmtOne (filter (not . isEmptyBlock) stmts)
  where
    isEmptyBlock (BlockStmt _ Nothing [] _) = True
    isEmptyBlock _ = False
    fmtOne s = do
        let sp = stmtLeadingStart s
            off = posOffset sp
            ln = posLine sp
        drainCommentsBefore off
        emitBlankLineSep ln
        newline
        fmtStmt s
        drainLineCommentAfter (posLine (spanEnd (stmtSpan s))) maxBound
        setLastLine (posLine (spanEnd (stmtSpan s)))

fmtTypeSwitchInit :: Stmt SrcSpan -> Printer ()
fmtTypeSwitchInit (AssignStmt _sp [v] InAssign [e]) = do
    fmtExpr v
    emit " in "
    fmtExpr e
fmtTypeSwitchInit s = fmtStmt s

fmtBlockBody :: Stmt SrcSpan -> Printer ()
fmtBlockBody (BlockStmt _sp _label [s] True) = do
    emit "do "
    fmtStmt s
fmtBlockBody (BlockStmt _sp _label stmts _isDo) = do
    emit "{"
    withIndent $ fmtStmtList stmts
    newline
    emit "}"
fmtBlockBody s = fmtStmt s

fmtSwitchBody :: Stmt SrcSpan -> Printer ()
fmtSwitchBody (BlockStmt _sp _label stmts _isDo) = do
    emit "{"
    fmtStmtList stmts
    newline
    emit "}"
fmtSwitchBody s = fmtStmt s

fmtAttribute :: Attribute SrcSpan -> Printer ()
fmtAttribute (Attribute sp exprs) = do
    emit "@"
    case exprs of
        [e] | isBareAttr sp -> fmtExpr e
        [e] -> parens (fmtExpr e)
        _ -> parens (commaSep (map fmtExpr exprs))
  where
    isBareAttr (SrcSpan _ (SrcPos 0 0 0)) = True
    isBareAttr _ = False

-- | Returns the source position of the first token printed for this statement.
-- For declarations with leading attributes the formatter prints attributes
-- before the name, but 'stmtSpan' only covers the name.  We therefore use the
-- first attribute's position when one is present, so that blank-line detection
-- and comment draining use the correct anchor point.
stmtLeadingStart :: Stmt SrcSpan -> SrcPos
stmtLeadingStart s = case s of
    ValueDecl _ attrs _ _ _ _ | not (null attrs) -> attrStart (last attrs)
    ImportDecl _ attrs _ _ | not (null attrs) -> attrStart (last attrs)
    ForeignImportDecl _ attrs _ _ | not (null attrs) -> attrStart (last attrs)
    ForeignBlockDecl _ attrs _ _ | not (null attrs) -> attrStart (last attrs)
    _ -> spanStart (stmtSpan s)
  where
    attrStart (Attribute sp _) = spanStart sp
