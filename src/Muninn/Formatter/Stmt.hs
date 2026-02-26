module Muninn.Formatter.Stmt
  ( fmtStmt
  , fmtExpr
  ) where

import Muninn.Parser.AST
import Muninn.Parser.JSON (showAssignOp, showBranchKind)
import Muninn.Parser.SrcLoc (SrcSpan)
import Muninn.Formatter.Print
import Muninn.Formatter.Expr (fmtExprWith)

fmtExpr :: Expr SrcSpan -> Printer ()
fmtExpr = fmtExprWith fmtStmt

fmtStmt :: Stmt SrcSpan -> Printer ()
fmtStmt = \case
  PackageDecl _sp name -> do
    emit "package "
    emit name

  ImportDecl _sp mAlias path -> do
    emit "import "
    case mAlias of
      Just alias -> do
        emit alias
        space
      Nothing -> pure ()
    emit "\""
    emit path
    emit "\""

  ForeignImportDecl _sp name paths -> do
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

  ForeignBlockDecl _sp mLib body -> do
    emit "foreign"
    case mLib of
      Just lib -> do
        space
        fmtExpr lib
      Nothing -> pure ()
    space
    fmtBlockBody body

  ValueDecl _sp attrs names mTy vals isMut -> do
    mapM_ (\a -> fmtAttribute a >> space) (reverse attrs)
    commaSep (map fmtExpr names)
    case (mTy, vals, isMut) of
      (Nothing, _:_, False) -> do
        emit " :: "
        commaSep (map fmtExpr vals)
      (Nothing, _:_, True) -> do
        emit " := "
        commaSep (map fmtExpr vals)
      (Just ty, _:_, False) -> do
        emit " : "
        fmtExpr ty
        emit " : "
        commaSep (map fmtExpr vals)
      (Just ty, _:_, True) -> do
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

  BlockStmt _sp mLabel stmts -> case mLabel of
    Just label -> do
      fmtExpr label
      emit ": "
      emit "{"
      withIndent $ mapM_ (\s -> newline >> fmtStmt s) stmts
      newline
      emit "}"
    Nothing -> case stmts of
      [] -> pure ()
      (s:ss) -> do
        fmtStmt s
        mapM_ (\s' -> newline >> fmtStmt s') ss

  IfStmt _sp mInit cond body mElse -> do
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

  ForStmt _sp mInit mCond mPost body -> do
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

  RangeStmt _sp vals range body rev -> do
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

  SwitchStmt _sp mInit mTag body partial -> do
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
    fmtBlockBody body

  TypeSwitchStmt _sp mInit mTag body partial -> do
    if partial then emit "#partial " else pure ()
    emit "switch "
    case mInit of
      Just ini -> do
        fmtTypeSwitchInit ini
        case mTag of
          Just _  -> emit "; "  -- real init before tag
          Nothing -> space      -- init IS the tag
      Nothing -> pure ()
    case mTag of
      Just tag -> fmtExpr tag >> space
      Nothing -> pure ()
    fmtBlockBody body

  CaseClause _sp exprs stmts -> do
    emit "case "
    commaSep (map fmtExpr exprs)
    emit ":"
    withIndent $ mapM_ (\s -> newline >> fmtStmt s) stmts

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

  BadStmt _sp -> emit "/* BAD STMT */"

-- Detect synthetic AssignStmt [v] InAssign [e] and format as "v in e"
fmtTypeSwitchInit :: Stmt SrcSpan -> Printer ()
fmtTypeSwitchInit (AssignStmt _sp [v] InAssign [e]) = do
  fmtExpr v
  emit " in "
  fmtExpr e
fmtTypeSwitchInit s = fmtStmt s

fmtBlockBody :: Stmt SrcSpan -> Printer ()
fmtBlockBody (BlockStmt _sp _label stmts) = do
  emit "{"
  withIndent $ mapM_ (\s -> newline >> fmtStmt s) stmts
  newline
  emit "}"
fmtBlockBody s = fmtStmt s

fmtAttribute :: Attribute SrcSpan -> Printer ()
fmtAttribute (Attribute _sp exprs) = do
  emit "@"
  case exprs of
    [e] -> parens (fmtExpr e)
    _   -> parens (commaSep (map fmtExpr exprs))
