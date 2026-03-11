module Muninn.Formatter.Expr (
    fmtExprWith,
) where

import Control.Monad (when)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Muninn.Formatter.Print
import Muninn.Parser.AST
import Muninn.Parser.JSON (
    showBinOp,
    showBranchKind,
    showCastKind,
    showStructFlag,
    showUnaryOp,
    showUnionFlag,
 )
import Muninn.Parser.SrcLoc (SrcPos (..), SrcSpan (..))

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
        BinaryExpr _sp lhs op rhs -> case op of
            OpRangeHalf -> go lhs >> space >> emit "..<" >> space >> go rhs
            OpRangeFull -> go lhs >> space >> emit "..=" >> space >> go rhs
            OpIn -> go lhs >> emit " in " >> go rhs
            OpNotIn -> go lhs >> emit " not_in " >> go rhs
            _ ->
                tryInline
                    (go lhs >> space >> emit (showBinOp op) >> space >> go rhs)
                    (fmtBinChain op (BinaryExpr _sp lhs op rhs))
        ParenExpr _sp e -> parens (go e)
        CallExpr sp fn args hasEllipsis -> do
            go fn
            let endOff = posOffset (spanEnd sp)
                fnEndLine = posLine (spanEnd (exprSpan fn))
                isSourceMultiLine = case args of
                    (firstArg : _) -> posLine (spanStart (exprSpan firstArg)) /= fnEndLine
                    [] -> False
                fmtMultiLine = do
                    emit "("
                    withIndent $ inCallArgs $
                        if hasEllipsis && not (null args)
                            then mapM_
                                (\(isEll, e) -> newline >> when isEll (emit "..") >> go e >> emit ",")
                                (insertEllipsis args)
                            else mapM_ (\e -> newline >> go e >> emit ",") args
                    newline
                    emit ")"
            hasComments <- hasCommentsBefore endOff
            if hasComments
                then do
                    emit "("
                    setLastLine 0
                    withIndent $ inCallArgs $ do
                        if hasEllipsis && not (null args)
                            then do
                                let argsList = insertEllipsis args
                                    nextStarts = map (posOffset . spanStart . exprSpan . snd) (drop 1 argsList) ++ [endOff]
                                mapM_
                                    ( \((isEll, e), nextStart) -> do
                                        let esp = exprSpan e
                                        drainCommentsBefore (posOffset (spanStart esp))
                                        newline
                                        when isEll (emit "..")
                                        go e
                                        emit ","
                                        setLastLine (posLine (spanEnd esp))
                                        drainLineCommentAfter (posLine (spanEnd esp)) nextStart
                                    )
                                    (zip argsList nextStarts)
                            else do
                                let nextStarts = map (posOffset . spanStart . exprSpan) (drop 1 args) ++ [endOff]
                                mapM_
                                    ( \(e, nextStart) -> do
                                        let esp = exprSpan e
                                        drainCommentsBefore (posOffset (spanStart esp))
                                        newline
                                        go e
                                        emit ","
                                        setLastLine (posLine (spanEnd esp))
                                        drainLineCommentAfter (posLine (spanEnd esp)) nextStart
                                    )
                                    (zip args nextStarts)
                        drainCommentsBefore endOff
                    newline
                    emit ")"
                else if isSourceMultiLine
                    then fmtMultiLine
                    else tryInline
                        ( parens $
                            if hasEllipsis && not (null args)
                                then commaSep (map (fmtCallArg go) (insertEllipsis args))
                                else commaSep (map go args)
                        )
                        fmtMultiLine
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
        SelectorExpr _sp e sel isArrow -> do
            go e
            if isArrow then emit "->" else emit "."
            go sel
        ImplicitSelectorExpr _sp sel -> do
            emit "."
            go sel
        DerefExpr _sp e -> do
            go e
            emit "^"
        CompLit sp mTy elems -> do
            maybe (pure ()) go mTy
            let endOff = posOffset (spanEnd sp)
                alignW = compLitAlignWidth elems
            hasComments <- hasCommentsBefore endOff
            if hasComments
                then do
                    emit "{"
                    setLastLine 0
                    withIndent $ do
                        let nextStarts = map (posOffset . spanStart . exprSpan) (drop 1 elems) ++ [endOff]
                        mapM_
                            ( \(e, nextStart) -> do
                                let esp = exprSpan e
                                drainCommentsBefore (posOffset (spanStart esp))
                                newline
                                fmtCompLitField alignW e
                                emit ","
                                drainLineCommentAfter (posLine (spanEnd esp)) nextStart
                                setLastLine (posLine (spanEnd esp))
                            )
                            (zip elems nextStarts)
                        drainCommentsBefore endOff
                    newline
                    emit "}"
                else if not (null elems) && all isImplicitSelector elems
                    then do
                        case mTy of
                            Just _ -> emit " {"
                            Nothing -> emit "{"
                        withIndent $
                            mapM_ (\e -> newline >> go e >> emit ",") elems
                        newline
                        emit "}"
                    else do
                        let isSourceMultiLine = posLine (spanStart sp) /= posLine (spanEnd sp)
                            fmtMultiLine = do
                                case mTy of
                                    Just _ -> emit " {"
                                    Nothing -> emit "{"
                                withIndent $ mapM_ (\e -> newline >> fmtCompLitField alignW e >> emit ",") elems
                                newline
                                emit "}"
                        if isSourceMultiLine
                            then fmtMultiLine
                            else
                                tryInline
                                    (do emit "{"; commaSep (map go elems); emit "}")
                                    fmtMultiLine
        ProcLit _sp mDir ty tags mBody -> do
            case mDir of
                Just d -> do emit "#"; emit d; space
                Nothing -> pure ()
            go ty
            mapM_ (\t -> do space; emit "#"; emit t) tags
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
            emit "proc {"
            setLastLine 0
            withIndent $ mapM_ (\e -> do newline; go e; emit ",") exprs
            newline
            emit "}"
        Ellipsis _sp mE -> do
            maybe (emit "...") (\e -> emit ".." >> go e) mE
        TernaryIfExpr _sp isQMark cond then_ else_ ->
            if isQMark
                then do
                    go cond
                    emit " ? "
                    go then_
                    emit " : "
                    go else_
                else do
                    go then_
                    emit " if "
                    go cond
                    emit " else "
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
        StructType sp mParams fields mAlign flags -> do
            emit "struct"
            mapM_ (\f -> emit " #" >> emit (showStructFlag f)) flags
            case mParams of
                Just params -> parens (fmtFieldListInline params)
                Nothing -> pure ()
            case mAlign of
                Just (ParenExpr _ a) -> do
                    emit " #align("
                    go a
                    emit ")"
                Just a -> do
                    emit " #align "
                    go a
                Nothing -> pure ()
            let FieldList _ flds = fields
            if null flds
                then -- Preserve source spacing: `struct{}` (anonymous type) vs `struct {}` (definition).
                     -- Plain `struct{}` has span length 8; anything with a space or extras is > 8.
                     if posOffset (spanEnd sp) - posOffset (spanStart sp) > 8
                         then emit " {}"
                         else emit "{}"
                else bracesBlock (fmtFieldListStruct fields)
        UnionType sp variants mParams flags -> do
            emit "union"
            mapM_ (\f -> emit " #" >> emit (showUnionFlag f)) flags
            case mParams of
                Just params -> parens (fmtFieldListInline params)
                Nothing -> pure ()
            if null variants
                then emit " {}"
                else bracesBlock (fmtVariants (posOffset (spanEnd sp)) variants)
        EnumType sp mBacking fields -> do
            emit "enum"
            case mBacking of
                Just b -> do
                    space
                    go b
                Nothing -> pure ()
            if null fields
                then emit " {}"
                else bracesBlock (fmtEnumFields (posOffset (spanEnd sp)) fields)
        BitSetType _sp e mUnderlying -> do
            emit "bit_set["
            go e
            case mUnderlying of
                Just u -> do
                    emit "; "
                    go u
                Nothing -> pure ()
            emit "]"
        BitFieldType sp mBacking fields -> do
            emit "bit_field"
            case mBacking of
                Just b -> do
                    space
                    go b
                Nothing -> pure ()
            bracesBlock (fmtBitFieldFields (posOffset (spanEnd sp)) fields)
        ProcType _sp params mResults isDiverging mCC whereExprs -> do
            emit "proc"
            case mCC of
                Just cc -> do
                    emit " \""
                    emit cc
                    emit "\" "
                Nothing -> pure ()
            let FieldList flsp fields = params
                alignW = computeAlignWidth fields
                isSourceMultiLine = posLine (spanStart flsp) /= posLine (spanEnd flsp)
                fmtResultSuffix = do
                    case (mResults, isDiverging) of
                        (Just results, _) -> do
                            emit " -> "
                            fmtResultType results
                        (Nothing, True) -> emit " -> !"
                        (Nothing, False) -> pure ()
                    case whereExprs of
                        [] -> pure ()
                        _ -> do
                            emit " where "
                            commaSep (map go whereExprs)
            if isSourceMultiLine
                then fmtFieldListMultiLine alignW params >> fmtResultSuffix
                else
                    tryInline
                        (do parens (fmtFieldListInline params); fmtResultSuffix)
                        (do fmtFieldListMultiLine alignW params; fmtResultSuffix)
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
    fmtBlockBody (BlockStmt sp _label stmts _isDo) = do
        let endOff = posOffset (spanEnd sp)
        emit " {"
        setLastLine 0
        drainLineCommentAfter (posLine (spanStart sp)) endOff
        withIndent $ do
            fmtStmtList stmts
            drainCommentsBefore endOff
        newline
        emit "}"
    fmtBlockBody s = do
        space
        fmtS s

    fmtStmtList :: [Stmt SrcSpan] -> Printer ()
    fmtStmtList stmts = do
        setLastLine 0
        mapM_ fmtOne (filter (not . isEmptyBlock) stmts)
      where
        isEmptyBlock (BlockStmt _ Nothing [] _) = True
        isEmptyBlock _ = False
        fmtOne s = do
            let sp = leadingStart s
                off = posOffset sp
                ln = posLine sp
            drainCommentsBefore off
            emitBlankLineSep ln
            newline
            fmtS s
            drainLineCommentAfter (posLine (spanEnd (stmtSpan s))) maxBound
            setLastLine (posLine (spanEnd (stmtSpan s)))
        leadingStart s = case s of
            ValueDecl _ attrs _ _ _ _ | not (null attrs) -> spanStart (attrAnn (last attrs))
            ImportDecl _ attrs _ _ | not (null attrs) -> spanStart (attrAnn (last attrs))
            ForeignImportDecl _ attrs _ _ | not (null attrs) -> spanStart (attrAnn (last attrs))
            ForeignBlockDecl _ attrs _ _ | not (null attrs) -> spanStart (attrAnn (last attrs))
            _ -> spanStart (stmtSpan s)

    isSyntheticUnderscore :: [Expr SrcSpan] -> Maybe (Expr SrcSpan) -> Bool
    isSyntheticUnderscore [Ident nsp "_"] (Just ty) =
        posOffset (spanStart nsp) == posOffset (spanStart (exprSpan ty))
    isSyntheticUnderscore _ _ = False

    -- Collect all operands of a left-associative binary chain with the same op.
    collectBinOps :: BinOp -> Expr SrcSpan -> [Expr SrcSpan]
    collectBinOps op (BinaryExpr _ lhs op' rhs) | op' == op = collectBinOps op lhs ++ [rhs]
    collectBinOps _ e = [e]

    -- Multi-line fallback for a binary chain: first operand on the current line,
    -- remaining operands each on a new indented line preceded by the operator.
    fmtBinChain :: BinOp -> Expr SrcSpan -> Printer ()
    fmtBinChain op e = do
        let operands = collectBinOps op e
            opStr = showBinOp op
        case operands of
            [] -> pure ()
            (first : rest) -> do
                go first
                withBinChainIndent $ mapM_ (\e' -> space >> emit opStr >> newline >> go e') rest

    fmtField :: Field SrcSpan -> Printer ()
    fmtField = fmtFieldWith False Nothing

    fmtFieldWith :: Bool -> Maybe Int -> Field SrcSpan -> Printer ()
    fmtFieldWith isStruct mAlignW f@(Field _sp names mTy mDef mTag flags) = do
        mapM_ fmtFieldFlag flags
        let unnamed = isSyntheticUnderscore names mTy
        if unnamed then pure () else commaSep (map go names)
        case (null names || unnamed, mTy, mDef) of
            (True, Just ty, _) -> go ty
            (False, Just ty, Just def) -> do
                emitColon isStruct mAlignW f
                go ty
                emit " = "
                go def
            (False, Just ty, Nothing) -> do
                emitColon isStruct mAlignW f
                go ty
            (False, Nothing, Just def) -> do
                emit " := "
                go def
            _ -> pure ()
        case mTag of
            Just tag -> do
                space
                emit tag
            Nothing -> pure ()

    emitColon :: Bool -> Maybe Int -> Field SrcSpan -> Printer ()
    emitColon _ Nothing _ = emit ": "
    emitColon _ (Just target) fld =
        case fieldPrefixWidth fld of
            Nothing -> emit ": "
            Just pw -> do
                emit ":"
                emit (T.replicate (max 1 (target - pw + 1)) " ")

    fmtFieldFlag :: FieldFlag -> Printer ()
    fmtFieldFlag = \case
        FieldUsing -> emit "using "
        FieldAnyInt -> emit "#any_int "
        FieldCVararg -> emit "#c_vararg "
        FieldNoAlias -> emit "#no_alias "
        FieldSubtype -> emit "#subtype "
        FieldConst -> emit "#const "
        FieldByPtr -> emit "#by_ptr "
        FieldNoBroadcast -> emit "#no_broadcast "

    fieldFlagWidth :: FieldFlag -> Int
    fieldFlagWidth = \case
        FieldUsing -> 6
        FieldAnyInt -> 9
        FieldCVararg -> 10
        FieldNoAlias -> 10
        FieldSubtype -> 9
        FieldConst -> 7
        FieldByPtr -> 8
        FieldNoBroadcast -> 15

    -- Raw name/flag width, without colon or trailing space.
    fieldPrefixBase :: Field SrcSpan -> Maybe Int
    fieldPrefixBase (Field _ names mTy _mDef _mTag flags)
        | isSyntheticUnderscore names mTy = Nothing
        | null names = Nothing
        | Nothing <- mTy = Nothing
        | otherwise = Just (flagsW + namesW + sepW)
      where
        flagsW = sum (map fieldFlagWidth flags)
        namesW = sum (map nameWidth names)
        sepW = max 0 ((length names - 1) * 2)
        nameWidth (Ident _ name) = T.length name
        nameWidth _ = 0

    -- Param-style prefix width: includes ": " (colon + space).
    fieldPrefixWidth :: Field SrcSpan -> Maybe Int
    fieldPrefixWidth = fmap (+ 2) . fieldPrefixBase

    -- Param-style alignment: types at the same column.
    computeAlignWidth :: [Field SrcSpan] -> Maybe Int
    computeAlignWidth fields =
        let widths = mapMaybe fieldPrefixWidth fields
         in if length widths >= 2
                then Just (maximum widths)
                else Nothing

    fmtFieldListStruct :: FieldList SrcSpan -> Printer ()
    fmtFieldListStruct (FieldList sp fields) = do
        let endOff = posOffset (spanEnd sp)
            alignW = computeAlignWidth fields
        mapM_
            ( \f -> do
                let fsp = fieldAnn f
                drainCommentsBefore (posOffset (spanStart fsp))
                newline
                fmtFieldWith False alignW f
                emit ","
                drainLineCommentAfter (posLine (spanEnd fsp)) maxBound
                setLastLine (posLine (spanEnd fsp))
            )
            fields
        drainCommentsBefore endOff

    fmtFieldListMultiLine :: Maybe Int -> FieldList SrcSpan -> Printer ()
    fmtFieldListMultiLine alignW (FieldList sp fields) = do
        let endOff = posOffset (spanEnd sp)
            nextStarts = map (posOffset . spanStart . fieldAnn) (drop 1 fields) ++ [endOff]
        emit "("
        setLastLine 0
        withIndent $
            mapM_
                ( \(f, nextStart) -> do
                    let fsp = fieldAnn f
                    drainCommentsBefore (posOffset (spanStart fsp))
                    newline
                    fmtFieldWith False alignW f
                    emit ","
                    drainLineCommentAfter (posLine (spanEnd fsp)) nextStart
                    setLastLine (posLine (spanEnd fsp))
                )
                (zip fields nextStarts)
        drainCommentsBefore endOff
        newline
        emit ")"

    fmtFieldListInline :: FieldList SrcSpan -> Printer ()
    fmtFieldListInline (FieldList sp fields) = do
        let endOff = posOffset (spanEnd sp)
        hasComments <- hasCommentsBefore endOff
        if hasComments
            then do
                let nextStarts = map (posOffset . spanStart . fieldAnn) (drop 1 fields) ++ [endOff]
                setLastLine 0
                withIndent $
                    mapM_
                        ( \(f, nextStart) -> do
                            let fsp = fieldAnn f
                            drainCommentsBefore (posOffset (spanStart fsp))
                            newline
                            fmtField f
                            emit ","
                            drainLineCommentAfter (posLine (spanEnd fsp)) nextStart
                            setLastLine (posLine (spanEnd fsp))
                        )
                        (zip fields nextStarts)
                drainCommentsBefore endOff
                newline
            else commaSep (map fmtField fields)

    fmtResultType :: FieldList SrcSpan -> Printer ()
    fmtResultType (FieldList _sp [Field _fsp [] (Just ty) Nothing Nothing []]) = go ty
    fmtResultType fl@(FieldList sp _) =
        if posLine (spanStart sp) /= posLine (spanEnd sp)
            then fmtFieldListMultiLine Nothing fl
            else
                tryInline
                    (parens (fmtFieldListInline fl))
                    (fmtFieldListMultiLine Nothing fl)

    fmtVariants :: Int -> [Expr SrcSpan] -> Printer ()
    fmtVariants endOff variants = do
        mapM_
            ( \v -> do
                let sp = exprSpan v
                drainCommentsBefore (posOffset (spanStart sp))
                newline
                go v
                emit ","
                drainLineCommentAfter (posLine (spanEnd sp)) maxBound
                setLastLine (posLine (spanEnd sp))
            )
            variants
        drainCommentsBefore endOff

    insertEllipsis :: [Expr SrcSpan] -> [(Bool, Expr SrcSpan)]
    insertEllipsis args =
        let (positional, named) = span (not . isFieldValue) args
         in case positional of
                [] -> case named of
                    [] -> []
                    _ -> map (\a -> (False, a)) named
                _ ->
                    map (\a -> (False, a)) (init positional)
                        ++ [(True, last positional)]
                        ++ map (\a -> (False, a)) named

    isFieldValue :: Expr SrcSpan -> Bool
    isFieldValue (FieldValue _ _ _) = True
    isFieldValue _ = False

    isImplicitSelector :: Expr SrcSpan -> Bool
    isImplicitSelector (ImplicitSelectorExpr _ _) = True
    isImplicitSelector _ = False

    fmtCallArg :: (Expr SrcSpan -> Printer ()) -> (Bool, Expr SrcSpan) -> Printer ()
    fmtCallArg fmt (True, e) = emit ".." >> fmt e
    fmtCallArg fmt (False, e) = fmt e

    compLitKeyWidth :: Expr SrcSpan -> Int
    compLitKeyWidth (Ident _ name) = T.length name
    compLitKeyWidth _ = 0

    compLitAlignWidth :: [Expr SrcSpan] -> Maybe Int
    compLitAlignWidth elems
        | all isFieldValue elems, length elems >= 2 =
            Just (maximum (mapMaybe fvKeyWidth elems))
        | otherwise = Nothing
      where
        fvKeyWidth (FieldValue _ k _) = Just (compLitKeyWidth k)
        fvKeyWidth _ = Nothing

    fmtCompLitField :: Maybe Int -> Expr SrcSpan -> Printer ()
    fmtCompLitField (Just target) (FieldValue _sp k v) = do
        go k
        let pad = max 1 (target - compLitKeyWidth k + 1)
        emit (T.replicate pad " ")
        emit "= "
        go v
    fmtCompLitField _ e = go e

    fmtEnumFields :: Int -> [Expr SrcSpan] -> Printer ()
    fmtEnumFields endOff fields = do
        let alignW = computeEnumAlignWidth fields
        mapM_
            ( \f -> do
                let sp = exprSpan f
                drainCommentsBefore (posOffset (spanStart sp))
                newline
                fmtEnumField alignW f
                emit ","
                drainLineCommentAfter (posLine (spanEnd sp)) maxBound
                setLastLine (posLine (spanEnd sp))
            )
            fields
        drainCommentsBefore endOff

    fmtEnumField :: Maybe Int -> Expr SrcSpan -> Printer ()
    fmtEnumField (Just target) (FieldValue _sp k v) = do
        go k
        let pad = max 1 (target - enumKeyWidth k + 1)
        emit (T.replicate pad " ")
        emit "= "
        go v
    fmtEnumField _ e = go e

    enumKeyWidth :: Expr SrcSpan -> Int
    enumKeyWidth (Ident _ name) = T.length name
    enumKeyWidth _ = 0

    computeEnumAlignWidth :: [Expr SrcSpan] -> Maybe Int
    computeEnumAlignWidth exprs =
        let fvWidths = mapMaybe nameW exprs
         in if length fvWidths >= 2
                then Just (maximum (map entryWidth exprs))
                else Nothing
      where
        nameW (FieldValue _ k _) = Just (enumKeyWidth k)
        nameW _ = Nothing
        entryWidth (FieldValue _ k _) = enumKeyWidth k
        entryWidth e = enumKeyWidth e

    fmtBitFieldFields :: Int -> [BitFieldField SrcSpan] -> Printer ()
    fmtBitFieldFields endOff fields = do
        mapM_
            ( \f -> do
                let sp = bffAnn f
                drainCommentsBefore (posOffset (spanStart sp))
                newline
                fmtBitFieldField f
                emit ","
                drainLineCommentAfter (posLine (spanEnd sp)) maxBound
                setLastLine (posLine (spanEnd sp))
            )
            fields
        drainCommentsBefore endOff

    fmtBitFieldField :: BitFieldField SrcSpan -> Printer ()
    fmtBitFieldField (BitFieldField _sp name ty size) = do
        go name
        emit ": "
        go ty
        emit " | "
        go size
