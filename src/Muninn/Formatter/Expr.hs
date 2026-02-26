module Muninn.Formatter.Expr
  ( fmtExprWith
  ) where

import Muninn.Parser.AST
import Muninn.Parser.JSON (showBinOp, showUnaryOp, showBranchKind, showCastKind,
                           showStructFlag, showUnionFlag)
import Muninn.Parser.SrcLoc (SrcSpan)
import Muninn.Formatter.Print

fmtExprWith :: (Stmt SrcSpan -> Printer ()) -> Expr SrcSpan -> Printer ()
fmtExprWith fmtS = go
  where
  go :: Expr SrcSpan -> Printer ()
  go = \case
    Ident _sp name -> emit name
    BasicLit _sp _kind val -> emit val
    Undef _sp -> emit "---"
    Implicit _sp -> emit "context"

    UnaryExpr _sp op mE -> do
      emit (showUnaryOp op)
      maybe (pure ()) go mE

    BinaryExpr _sp lhs op rhs -> do
      go lhs
      case op of
        OpRangeHalf -> emit "..<"
        OpRangeFull -> emit "..="
        OpIn -> emit " in "
        OpNotIn -> emit " not_in "
        _ -> do
          space
          emit (showBinOp op)
          space
      go rhs

    ParenExpr _sp e -> parens (go e)

    CallExpr _sp fn args hasEllipsis -> do
      go fn
      parens $ do
        if hasEllipsis && not (null args)
          then commaSep (map (fmtCallArg go) (insertEllipsis args))
          else commaSep (map go args)

    IndexExpr _sp e idx -> do
      go e
      brackets (go idx)

    MatrixIndexExpr _sp e row col -> do
      go e
      brackets $ do
        go row
        emit ", "
        go col

    SliceExpr _sp e lo hi -> do
      go e
      brackets $ do
        maybe (pure ()) go lo
        emit ":"
        maybe (pure ()) go hi

    SelectorExpr _sp e sel -> do
      go e
      emit "."
      go sel

    ImplicitSelectorExpr _sp sel -> do
      emit "."
      go sel

    DerefExpr _sp e -> do
      go e
      emit "^"

    CompLit _sp mTy elems -> do
      maybe (pure ()) go mTy
      emit "{"
      commaSep (map go elems)
      emit "}"

    ProcLit _sp ty mBody -> do
      go ty
      case mBody of
        Just body -> fmtBlockBody body
        Nothing -> do
          space
          emit "---"

    FieldValue _sp k v -> do
      go k
      emit " = "
      go v

    ProcGroup _sp exprs -> do
      emit "proc{"
      commaSep (map go exprs)
      emit "}"

    Ellipsis _sp mE -> do
      emit ".."
      maybe (pure ()) go mE

    -- Use ?: syntax (if/else form disallows comp lits in conditions)
    TernaryIfExpr _sp cond then_ else_ -> do
      go cond
      emit " ? "
      go then_
      emit " : "
      go else_

    TernaryWhenExpr _sp cond then_ else_ -> do
      go then_
      emit " when "
      go cond
      emit " else "
      go else_

    OrElseExpr _sp e1 e2 -> do
      go e1
      emit " or_else "
      go e2

    OrReturnExpr _sp e -> do
      go e
      emit " or_return"

    OrBranchExpr _sp e kind mLabel -> do
      go e
      space
      emit ("or_" <> showBranchKind kind)
      case mLabel of
        Just l -> do
          space
          go l
        Nothing -> pure ()

    TypeAssertion _sp e (UnaryExpr _ OpQuestion Nothing) -> do
      go e
      emit ".?"
    TypeAssertion _sp e ty -> do
      go e
      emit ".("
      go ty
      emit ")"

    TypeCast _sp kind ty e -> do
      emit (showCastKind kind)
      parens (go ty)
      go e

    AutoCast _sp e -> do
      emit "auto_cast "
      go e

    BasicDirective _sp name mE -> do
      emit "#"
      emit name
      case mE of
        Just e -> parens (go e)
        Nothing -> pure ()

    TagExpr _sp tag e -> do
      emit "#"
      emit tag
      space
      go e

    PointerType _sp e mTag -> do
      maybe (pure ()) (\t -> go t >> space) mTag
      emit "^"
      go e

    MultiPointerType _sp e -> do
      emit "[^]"
      go e

    ArrayType _sp mLen e mTag -> do
      maybe (pure ()) (\t -> go t >> space) mTag
      emit "["
      maybe (pure ()) go mLen
      emit "]"
      go e

    DynamicArrayType _sp e mTag -> do
      maybe (pure ()) (\t -> go t >> space) mTag
      emit "[dynamic]"
      go e

    MapType _sp k v -> do
      emit "map["
      go k
      emit "]"
      go v

    StructType _sp fields mAlign flags -> do
      emit "struct"
      mapM_ (\f -> emit " #" >> emit (showStructFlag f)) flags
      case mAlign of
        Just (ParenExpr _ a) -> do
          emit " #align("
          go a
          emit ")"
        Just a -> do
          emit " #align "
          go a
        Nothing -> pure ()
      bracesBlock (fmtFieldListStruct fields)

    UnionType _sp variants mParams flags -> do
      emit "union"
      mapM_ (\f -> emit " #" >> emit (showUnionFlag f)) flags
      case mParams of
        Just params -> parens (fmtFieldListInline params)
        Nothing -> pure ()
      bracesBlock (fmtVariants variants)

    EnumType _sp mBacking fields -> do
      emit "enum"
      case mBacking of
        Just b -> do
          space
          go b
        Nothing -> pure ()
      bracesBlock (fmtEnumFields fields)

    BitSetType _sp e mUnderlying -> do
      emit "bit_set["
      go e
      case mUnderlying of
        Just u -> do
          emit "; "
          go u
        Nothing -> pure ()
      emit "]"

    BitFieldType _sp mBacking fields -> do
      emit "bit_field"
      case mBacking of
        Just b -> do
          space
          go b
        Nothing -> pure ()
      bracesBlock (fmtBitFieldFields fields)

    ProcType _sp params mResults mCC -> do
      emit "proc"
      case mCC of
        Just cc -> do
          emit " \""
          emit cc
          emit "\" "
        Nothing -> pure ()
      parens (fmtFieldListInline params)
      case mResults of
        Just results -> do
          emit " -> "
          fmtResultType results
        Nothing -> pure ()

    MatrixType _sp rows cols elem_ -> do
      emit "matrix["
      go rows
      emit ", "
      go cols
      emit "]"
      go elem_

    DistinctType _sp e -> do
      emit "distinct "
      go e

    PolyType _sp name mSpec -> do
      emit "$"
      go name
      case mSpec of
        Just spec -> do
          emit "/"
          go spec
        Nothing -> pure ()

    TypeidType _sp mSpec -> do
      emit "typeid"
      case mSpec of
        Just spec -> do
          emit "/"
          go spec
        Nothing -> pure ()

    HelperType _sp e -> do
      emit "#type "
      go e

    RelativeType _sp tag ty -> do
      go tag
      emit " "
      go ty

    InlineAsmExpr _sp params ret -> do
      emit "asm"
      parens (go params)
      emit " -> "
      go ret

    BadExpr _sp -> emit "/* BAD EXPR */"

  fmtBlockBody :: Stmt SrcSpan -> Printer ()
  fmtBlockBody (BlockStmt _sp _label stmts) = do
    bracesBlock (mapM_ (\s -> newline >> fmtS s) stmts)
  fmtBlockBody s = do
    space
    fmtS s

  fmtField :: Field SrcSpan -> Printer ()
  fmtField (Field _sp names mTy mDef mTag flags) = do
    mapM_ fmtFieldFlag flags
    commaSep (map go names)
    case (null names, mTy, mDef) of
      (True, Just ty, _) -> go ty
      (False, Just ty, Just def) -> do
        emit ": "
        go ty
        emit " = "
        go def
      (False, Just ty, Nothing) -> do
        emit ": "
        go ty
      (False, Nothing, Just def) -> do
        emit ": = "
        go def
      _ -> pure ()
    case mTag of
      Just tag -> do
        space
        emit tag
      Nothing -> pure ()

  fmtFieldFlag :: FieldFlag -> Printer ()
  fmtFieldFlag = \case
    FieldUsing    -> emit "using "
    FieldAnyInt   -> emit "#any_int "
    FieldCVararg  -> emit "#c_vararg "
    FieldNoAlias  -> emit "#no_alias "
    FieldSubtype  -> emit "#subtype "
    FieldConst    -> emit "#const "
    FieldByPtr    -> emit "#by_ptr "
    FieldNoBroadcast -> emit "#no_broadcast "

  fmtFieldListStruct :: FieldList SrcSpan -> Printer ()
  fmtFieldListStruct (FieldList _sp fields) =
    mapM_ (\f -> newline >> fmtField f >> emit ",") fields

  fmtFieldListInline :: FieldList SrcSpan -> Printer ()
  fmtFieldListInline (FieldList _sp fields) =
    commaSep (map fmtField fields)

  fmtResultType :: FieldList SrcSpan -> Printer ()
  fmtResultType (FieldList _sp [Field _fsp [] (Just ty) Nothing Nothing []]) = go ty
  fmtResultType fl = parens (fmtFieldListInline fl)

  fmtVariants :: [Expr SrcSpan] -> Printer ()
  fmtVariants variants =
    mapM_ (\v -> newline >> go v >> emit ",") variants

  -- Re-insert ".." before the last positional arg (parser strips Ellipsis wrapper)
  insertEllipsis :: [Expr SrcSpan] -> [(Bool, Expr SrcSpan)]
  insertEllipsis args =
    let (positional, named) = span (not . isFieldValue) args
    in case positional of
      [] -> case named of
        [] -> []
        _  -> map (\a -> (False, a)) named  -- shouldn't happen: ellipsis with only named args
      _  -> map (\a -> (False, a)) (init positional)
             ++ [(True, last positional)]
             ++ map (\a -> (False, a)) named

  isFieldValue :: Expr SrcSpan -> Bool
  isFieldValue (FieldValue _ _ _) = True
  isFieldValue _ = False

  fmtCallArg :: (Expr SrcSpan -> Printer ()) -> (Bool, Expr SrcSpan) -> Printer ()
  fmtCallArg fmt (True, e)  = emit ".." >> fmt e
  fmtCallArg fmt (False, e) = fmt e

  fmtEnumFields :: [Expr SrcSpan] -> Printer ()
  fmtEnumFields fields =
    mapM_ (\f -> newline >> go f >> emit ",") fields

  fmtBitFieldFields :: [BitFieldField SrcSpan] -> Printer ()
  fmtBitFieldFields fields =
    mapM_ (\f -> newline >> fmtBitFieldField f >> emit ",") fields

  fmtBitFieldField :: BitFieldField SrcSpan -> Printer ()
  fmtBitFieldField (BitFieldField _sp name ty size) = do
    go name
    emit ": "
    go ty
    emit " | "
    go size
