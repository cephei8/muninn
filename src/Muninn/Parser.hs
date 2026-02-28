module Muninn.Parser (
    parseFile,
    parseOdin,
    stmtSpan,
    exprSpan,
) where

import Control.Monad (guard, void, when)
import Control.Monad.State.Strict (get, modify, put)
import Control.Monad.Trans (lift)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Megaparsec (choice, many, option, optional, try, (<?>), (<|>))
import Text.Megaparsec qualified as MP

import Muninn.Parser.AST
import Muninn.Parser.Lexer
import Muninn.Parser.SrcLoc (SrcPos (..), SrcSpan (..))

parseFile :: FilePath -> IO (Either String (File SrcSpan))
parseFile path = do
    contents <- TIO.readFile path
    pure $ parseOdin path contents

parseOdin :: FilePath -> Text -> Either String (File SrcSpan)
parseOdin = runMuninnParser pFile

{- | In Odin's parser, a bare 'typeid' in a parameter position gets end==pos due to a
tokenizer look-ahead quirk.  This does NOT apply in result positions.
Collapse the TypeidType span and adjust the field's own end accordingly.
-}
fixParamTypeid :: Field SrcSpan -> Field SrcSpan
fixParamTypeid f@(Field (SrcSpan fs _fe) names mty Nothing tag flags) =
    case mty of
        Just (TypeidType (SrcSpan s _e) Nothing) ->
            let ty' = TypeidType (SrcSpan s s) Nothing
             in Field (SrcSpan fs s) names (Just ty') Nothing tag flags
        _ -> f
fixParamTypeid f = f

fieldSpan :: SrcPos -> Maybe (Expr SrcSpan) -> Maybe (Expr SrcSpan) -> SrcSpan
fieldSpan start ty dflt = SrcSpan start end
  where
    end = case dflt of
        Just d -> spanEnd (exprSpan d)
        Nothing -> case ty of
            Just t -> spanEnd (exprSpan t)
            Nothing -> start

-- Megaparsec's 'try' only backtracks the stream position, not monadic State.
tryRestore :: Parser () -> Parser a -> Parser a
tryRestore restore p = do
    r <- optional (try p)
    case r of
        Just x -> pure x
        Nothing -> restore *> MP.empty

pFile :: Parser (File SrcSpan)
pFile = do
    start <- getPos
    atEof <- MP.atEnd
    if atEof
        then do
            st <- lift get
            let zeroPos = SrcPos 0 0 0
            let comments = sortOn (\(CommentGroup sp _) -> posOffset (spanStart sp)) (psCommentGroups st)
            pure $ File (SrcSpan zeroPos zeroPos) "" [] comments Nothing
        else do
            st0 <- lift get
            let docs = psLeadComment st0
            keyword "package"
            pkg <- ident
            -- Odin parser bug: parse_file() rejects reserved package names ("builtin",
            -- "intrinsics"). We match this: return File with zeroed span and empty decls.
            if pkg `elem` ["builtin", "intrinsics"]
                then do
                    void MP.takeRest
                    st <- lift get
                    let zeroPos = SrcPos 0 0 0
                    let comments = sortOn (\(CommentGroup sp _) -> posOffset (spanStart sp)) (psCommentGroups st)
                    pure $ File (SrcSpan zeroPos zeroPos) pkg [] comments docs
                else do
                    semi
                    decls <- many pTopDecl
                    sp <- spanFrom start
                    st <- lift get
                    let comments = sortOn (\(CommentGroup csp _) -> posOffset (spanStart csp)) (psCommentGroups st)
                    pure $ File sp pkg decls comments docs

pTopDecl :: Parser (Stmt SrcSpan)
pTopDecl =
    choice
        [ tryState (pImportDecl <* semi)
        , tryState pAttributedTopDecl
        , tryState (pWhenStmt <* semi)
        , tryState (pForeignBlock <* semi)
        , tryState (pForeignImport <* semi)
        , pSimpleStmt <* semi
        ]

pAttributedTopDecl :: Parser (Stmt SrcSpan)
pAttributedTopDecl = do
    attrs <- some1 pAttribute
    _ <- optional semi
    target <- pAttrTarget
    semi
    pure $ applyAttrs attrs target

pAttrTarget :: Parser (Stmt SrcSpan)
pAttrTarget =
    choice
        [ try pForeignBlock
        , try pForeignImport
        , try pImportDecl
        , pValueDeclStmt
        ]

pImportDecl :: Parser (Stmt SrcSpan)
pImportDecl = do
    start <- getPos
    keyword "import"
    (alias, path) <- try aliasedImport <|> simpleImport
    sp <- spanFrom start
    pure $ ImportDecl sp alias path
  where
    aliasedImport = do
        alias <- ident
        path <- pString
        pure (Just alias, path)
    simpleImport = do
        path <- pString
        pure (Nothing, path)

pForeignBlock :: Parser (Stmt SrcSpan)
pForeignBlock = do
    start <- getPos
    keyword "foreign"
    foreignEnd <- endPos
    lib <- optional (try pExprNoCompLit)
    let lib' = case lib of
            Just e -> Just e
            Nothing -> Just (Ident (SrcSpan start foreignEnd) "_")
    body <- pBlock
    sp <- spanFrom start
    pure $ ForeignBlockDecl sp lib' body

pForeignImport :: Parser (Stmt SrcSpan)
pForeignImport = do
    start <- getPos
    keyword "foreign"
    keyword "import"
    (name, paths) <- try namedImport <|> unnamedImport
    sp <- spanFrom start
    pure $ ForeignImportDecl sp name paths
  where
    namedImport = do
        name <- ident
        paths <- singlePath <|> multiPath
        pure (name, paths)
    unnamedImport = do
        path <- pString
        pure ("", [path])
    singlePath = do
        path <- pString
        pure [path]
    multiPath = do
        lbrace
        items <- pathItem `sepBy1` comma
        _ <- optional comma
        rbrace
        pure [p | Just p <- items]
    pathItem = do
        base <- (Just <$> pString) <|> (Nothing <$ ident)
        hasWhen <- optional $ try $ do
            keyword "when"
            _ <- pExpr
            keyword "else"
            _ <- pString <|> ident
            pure ()
        case hasWhen of
            Just _ -> pure Nothing
            Nothing -> pure base

pSimpleStmt :: Parser (Stmt SrcSpan)
pSimpleStmt =
    choice
        [ try pAttributedValueDecl
        , try pForeignBlock
        , try pForeignImport
        , try pUsingStmt
        , pStmt
        ]

pAttributedValueDecl :: Parser (Stmt SrcSpan)
pAttributedValueDecl = do
    attrs <- some1 pAttribute
    _ <- optional semi
    target <-
        choice
            [ try pForeignBlock
            , try pForeignImport
            , pValueDeclStmt
            ]
    pure $ applyAttrs attrs target

pStmt :: Parser (Stmt SrcSpan)
pStmt =
    choice
        [ pBlock
        , tryState pSwitchStmt -- before pDirectiveBlock so #partial switch is handled correctly
        , tryState pDirectiveBlock
        , tryState pLabeledStmt
        , tryState pIfStmt
        , tryState pWhenStmt
        , tryState pForStmt
        , pReturnStmt
        , pDeferStmt
        , pBranchStmt
        , pExprOrDeclOrAssign
        ]

pDirectiveBlock :: Parser (Stmt SrcSpan)
pDirectiveBlock = do
    start <- getPos
    op "#"
    name <- ident
    case name of
        "no_bounds_check" -> pDirectiveBody start name
        "bounds_check" -> pDirectiveBody start name
        "no_type_assert" -> pDirectiveBody start name
        "type_assert" -> pDirectiveBody start name
        "force_inline" -> pExprOrDeclOrAssign
        "force_no_inline" -> pExprOrDeclOrAssign
        "reverse" -> do
            body <- pForStmt
            case body of
                RangeStmt sp lbl vals range bdy _ -> pure $ RangeStmt sp lbl vals range bdy True
                _ -> pure body
        "unroll" -> pForStmt
        _ -> fail $ "not a statement directive"
  where
    pDirectiveBody _start _name = do
        choice [pBlock, pForStmt, pSwitchStmt, pIfStmt, pWhenStmt, pReturnStmt, pExprOrDeclOrAssign]

pLabeledStmt :: Parser (Stmt SrcSpan)
pLabeledStmt = do
    savedSt <- lift get
    labelStart <- getPos
    label <- ident
    labelEnd <- endPos
    colon
    result <- optional $ choice [try pForStmt, try pSwitchStmt, try pBlock, try pIfStmt, try pDirectiveBlock]
    case result of
        Just stmt -> do
            let labelExpr = Ident (SrcSpan labelStart labelEnd) label
            pure $ applyLabel labelExpr stmt
        Nothing -> do
            lift $ put savedSt
            fail "expected labeled statement"
  where
    applyLabel lbl (BlockStmt sp _ stmts) = BlockStmt sp (Just lbl) stmts
    applyLabel lbl (ForStmt sp _ ini cond post body) = ForStmt sp (Just lbl) ini cond post body
    applyLabel lbl (RangeStmt sp _ vals range body rev) = RangeStmt sp (Just lbl) vals range body rev
    applyLabel lbl (SwitchStmt sp _ ini tag body partial) = SwitchStmt sp (Just lbl) ini tag body partial
    applyLabel lbl (TypeSwitchStmt sp _ ini tag body partial) = TypeSwitchStmt sp (Just lbl) ini tag body partial
    applyLabel lbl (IfStmt sp _ ini cond body els) = IfStmt sp (Just lbl) ini cond body els
    applyLabel _ s = s

pExprOrDeclOrAssign :: Parser (Stmt SrcSpan)
pExprOrDeclOrAssign = do
    exprs <- pExprList
    choice
        [ pDeclRest exprs
        , pAssignRest exprs
        , case exprs of
            [e] -> pure $ ExprStmt (exprSpan e) e
            _ -> fail "expected declaration or assignment"
        ]

pDeclRest :: [Expr SrcSpan] -> Parser (Stmt SrcSpan)
pDeclRest names = try $ do
    let nameStart = spanStart (listSpan names)
    colon
    choice
        [ do
            colon
            vals <- pExprList
            ep <- endPos
            pure $ ValueDecl (SrcSpan nameStart ep) [] names Nothing vals False
        , do
            op "="
            vals <- pExprList
            ep <- endPos
            pure $ ValueDecl (SrcSpan nameStart ep) [] names Nothing vals True
        , do
            ty <- pExpr
            choice
                [ do
                    colon
                    vals <- pExprList
                    ep <- endPos
                    pure $ ValueDecl (SrcSpan nameStart ep) [] names (Just ty) vals False
                , do
                    op "="
                    vals <- pExprList
                    ep <- endPos
                    pure $ ValueDecl (SrcSpan nameStart ep) [] names (Just ty) vals True
                , do
                    ep <- endPos
                    pure $ ValueDecl (SrcSpan nameStart ep) [] names (Just ty) [] True
                ]
        ]

pAssignRest :: [Expr SrcSpan] -> Parser (Stmt SrcSpan)
pAssignRest lhs = try $ do
    aop <- pAssignOp
    rhs <- pExprList
    pure $ AssignStmt (listSpan lhs <-> listSpan rhs) lhs aop rhs

pAssignRestNoCompLit :: [Expr SrcSpan] -> Parser (Stmt SrcSpan)
pAssignRestNoCompLit lhs = try $ do
    aop <- pAssignOp
    rhs <- pExprNoCompLit `sepBy1` comma
    pure $ AssignStmt (listSpan lhs <-> listSpan rhs) lhs aop rhs

pAssignOp :: Parser AssignOp
pAssignOp =
    choice
        [ AddAssign <$ try (op "+=")
        , SubAssign <$ try (op "-=")
        , MulAssign <$ try (op "*=")
        , QuoAssign <$ try (op "/=")
        , ModModAssign <$ try (op "%%=")
        , ModAssign <$ try (op "%=")
        , BitAndNotAssign <$ try (op "&~=")
        , BitAndAssign <$ try (op "&=")
        , BitOrAssign <$ try (op "|=")
        , BitXorAssign <$ try (op "~=")
        , ShlAssign <$ try (op "<<=")
        , ShrAssign <$ try (op ">>=")
        , LogAndAssign <$ try (op "&&=")
        , LogOrAssign <$ try (op "||=")
        , Assign <$ op "="
        ]

pValueDeclStmt :: Parser (Stmt SrcSpan)
pValueDeclStmt = do
    names <- pExprList
    pDeclRest names

pBlock :: Parser (Stmt SrcSpan)
pBlock = do
    start <- getPos
    clearPendingSemi
    -- Reset psExprLevel so auto-semicolons work inside blocks nested in (...) or [...]
    savedExprLevel <- psExprLevel <$> lift get
    lift $ modify $ \s -> s{psExprLevel = 0}
    lbrace
    stmts <- many (pBlockStmt <* semi)
    clearPendingSemi
    rbrace
    lift $ modify $ \s -> s{psExprLevel = savedExprLevel}
    sp <- spanFrom start
    pure $ BlockStmt sp Nothing stmts

-- Must save/restore PState since pBlock modifies state (clearPendingSemi) before
-- potentially failing at lbrace.
pBlockOrDo :: Parser (Stmt SrcSpan)
pBlockOrDo = do
    savedSt <- lift get
    choice
        [ pBlock
        , do
            lift $ put savedSt
            keyword "do"
            stmt <- pSimpleStmt
            let sp = stmtSpan stmt
            lift $ modify $ \s -> s{psEndPos = spanEnd sp}
            pure $ BlockStmt sp Nothing [stmt]
        ]

pBlockStmt :: Parser (Stmt SrcSpan)
pBlockStmt = pSimpleStmt

pIfStmt :: Parser (Stmt SrcSpan)
pIfStmt = do
    start <- getPos
    keyword "if"
    exprs <- pExprNoCompLit `sepBy1` comma
    choice
        [ try $ do
            initStmt <- pDeclRest exprs
            semi
            cond <- pExprNoCompLit
            body <- pBlockOrDo
            els <- pOptionalElse
            sp <- spanFrom start
            pure $ IfStmt sp Nothing (Just initStmt) cond body els
        , try $ do
            initStmt <- pAssignRest exprs
            semi
            cond <- pExprNoCompLit
            body <- pBlockOrDo
            els <- pOptionalElse
            sp <- spanFrom start
            pure $ IfStmt sp Nothing (Just initStmt) cond body els
        , case exprs of
            [cond] -> do
                body <- pBlockOrDo
                els <- pOptionalElse
                sp <- spanFrom start
                pure $ IfStmt sp Nothing Nothing cond body els
            _ -> fail "expected single if condition"
        ]

-- After pBlock, '}' sets psPendingSemi. Clear to allow 'else' on next line,
-- but restore if no else found so outer pBlockStmt <* semi can consume it.
pOptionalElse :: Parser (Maybe (Stmt SrcSpan))
pOptionalElse = do
    hadPendingSemi <- psPendingSemi <$> lift get
    clearPendingSemi
    els <- optional pElseClause
    case els of
        Nothing -> when hadPendingSemi (lift $ modify $ \s -> s{psPendingSemi = True})
        Just _ -> pure ()
    pure els

pElseClause :: Parser (Stmt SrcSpan)
pElseClause = do
    keyword "else"
    choice
        [ pBlock
        , pIfStmt
        , pWhenStmt
        ]

pWhenStmt :: Parser (Stmt SrcSpan)
pWhenStmt = do
    start <- getPos
    keyword "when"
    cond <- pExprNoCompLit
    body <- pBlockOrDo
    hadPendingSemi <- psPendingSemi <$> lift get
    clearPendingSemi
    els <- optional (keyword "else" *> (pBlock <|> pWhenStmt))
    case els of
        Nothing -> when hadPendingSemi (lift $ modify $ \s -> s{psPendingSemi = True})
        Just _ -> pure ()
    sp <- spanFrom start
    pure $ WhenStmt sp cond body els

pForStmt :: Parser (Stmt SrcSpan)
pForStmt = do
    start <- getPos
    keyword "for"
    savedSt <- lift get
    let restore = lift $ put savedSt
    choice
        [ do
            body <- pBlock
            sp <- spanFrom start
            pure $ ForStmt sp Nothing Nothing Nothing Nothing body
        , tryRestore restore $ do
            keyword "in"
            range <- pExprNoCompLit
            body <- pBlockOrDo
            sp <- spanFrom start
            pure $ RangeStmt sp Nothing [] range body False
        , tryRestore restore $ do
            vals <- pForInVar `sepBy1` comma
            keyword "in"
            range <- pExprNoCompLit
            body <- pBlockOrDo
            sp <- spanFrom start
            pure $ RangeStmt sp Nothing vals range body False
        , tryRestore restore $ do
            forSemi
            cond <- optional pExprNoCompLit
            forSemi
            post <- optional pForPost
            body <- pBlockOrDo
            sp <- spanFrom start
            pure $ ForStmt sp Nothing Nothing cond post body
        , tryRestore restore $ do
            ini <- pForInit
            forSemi
            cond <- optional pExprNoCompLit
            forSemi
            post <- optional pForPost
            body <- pBlockOrDo
            sp <- spanFrom start
            pure $ ForStmt sp Nothing (Just ini) cond post body
        , do
            cond <- pExprNoCompLit
            body <- pBlockOrDo
            sp <- spanFrom start
            pure $ ForStmt sp Nothing Nothing (Just cond) Nothing body
        ]

forSemi :: Parser ()
forSemi = do
    lift $ modify $ \s -> s{psPendingSemi = False, psAutoSemi = False}
    semi

pForInVar :: Parser (Expr SrcSpan)
pForInVar = do
    start <- getPos
    ref <- option False (True <$ op "&")
    nameStart <- getPos
    name <- ident
    nameEnd <- endPos
    let base = Ident (SrcSpan nameStart nameEnd) name
    if ref
        then do
            sp <- spanFrom start
            pure $ UnaryExpr sp OpAddr (Just base)
        else pure base

pForInit :: Parser (Stmt SrcSpan)
pForInit = do
    exprs <- pExprNoCompLit `sepBy1` comma
    choice
        [ pDeclRest exprs
        , pAssignRestNoCompLit exprs
        , case exprs of
            [e] -> pure $ ExprStmt (exprSpan e) e
            _ -> fail "expected single expression or declaration"
        ]

pForPost :: Parser (Stmt SrcSpan)
pForPost = do
    exprs <- pExprNoCompLit `sepBy1` comma
    choice
        [ pAssignRestNoCompLit exprs
        , case exprs of
            [e] -> pure $ ExprStmt (exprSpan e) e
            _ -> fail "expected single expression or assignment"
        ]

pSwitchStmt :: Parser (Stmt SrcSpan)
pSwitchStmt = do
    partial <- pPartialPrefix
    mLabel <- if partial then pPartialLabel else pure Nothing
    start <- getPos
    keyword "switch"
    choice
        [ try $ do
            body <- pSwitchBody
            sp <- spanFrom start
            pure $ SwitchStmt sp mLabel Nothing Nothing body partial
        , try $ do
            keyword "in"
            range <- pExprNoCompLit
            body <- pSwitchBody
            sp <- spanFrom start
            let wildcard = Ident (SrcSpan start start) "_"
            let initSp = SrcSpan start (spanEnd (exprSpan range))
            let initStmt = AssignStmt initSp [wildcard] InAssign [range]
            pure $ TypeSwitchStmt sp mLabel (Just initStmt) Nothing body partial
        , do
            exprs <- pExprNoCompLit `sepBy1` comma
            choice
                [ try $ do
                    initStmt <- pDeclRest exprs
                    semi
                    tag <- optional (try pExprNoCompLit)
                    body <- pSwitchBody
                    sp <- spanFrom start
                    pure $ SwitchStmt sp mLabel (Just initStmt) tag body partial
                , try $ do
                    initStmt <- pAssignRest exprs
                    semi
                    tag <- optional (try pExprNoCompLit)
                    body <- pSwitchBody
                    sp <- spanFrom start
                    pure $ SwitchStmt sp mLabel (Just initStmt) tag body partial
                , case exprs of
                    [BinaryExpr binSp lhs OpIn rhs] -> do
                        body <- pSwitchBody
                        sp <- spanFrom start
                        let initStmt = AssignStmt binSp [lhs] InAssign [rhs]
                        pure $ TypeSwitchStmt sp mLabel (Just initStmt) Nothing body partial
                    [e] -> do
                        body <- pSwitchBody
                        sp <- spanFrom start
                        pure $ SwitchStmt sp mLabel Nothing (Just e) body partial
                    _ -> fail "expected single switch expression"
                ]
        ]
  where
    pPartialPrefix = try (True <$ (op "#" *> ident' "partial")) <|> pure False
    pPartialLabel = optional $ try $ do
        labelStart <- getPos
        name <- ident
        labelEnd <- endPos
        colon
        pure $ Ident (SrcSpan labelStart labelEnd) name
    ident' expected = do
        name <- ident
        if name == expected then pure () else fail $ "expected " ++ T.unpack expected

pHashIdent :: Text -> Parser Text
pHashIdent expected = try $ do
    op "#"
    name <- ident
    if name == expected
        then pure name
        else fail $ "expected #" ++ T.unpack expected

pSwitchBody :: Parser (Stmt SrcSpan)
pSwitchBody = do
    start <- getPos
    clearPendingSemi
    savedExprLevel <- psExprLevel <$> lift get
    lift $ modify $ \s -> s{psExprLevel = 0}
    lbrace
    clauses <- many pCaseClause
    clearPendingSemi
    rbrace
    lift $ modify $ \s -> s{psExprLevel = savedExprLevel}
    sp <- spanFrom start
    pure $ BlockStmt sp Nothing clauses

pCaseClause :: Parser (Stmt SrcSpan)
pCaseClause = do
    start <- getPos
    keyword "case"
    exprs <- option [] (try pExprList)
    colon
    stmts <- many (pBlockStmt <* semi)
    sp <- spanFrom start
    pure $ CaseClause sp exprs stmts

pReturnStmt :: Parser (Stmt SrcSpan)
pReturnStmt = do
    start <- getPos
    keyword "return"
    st <- lift get
    if psPendingSemi st
        then do
            sp <- spanFrom start
            pure $ ReturnStmt sp []
        else do
            vals <- option [] (try pExprList)
            -- Save end pos before trailing comma so ReturnStmt span doesn't include it
            savedEnd <- endPos
            tc <- optional (try comma)
            case tc of
                Just _ -> lift $ modify $ \s -> s{psAutoSemi = True, psEndPos = savedEnd}
                Nothing -> pure ()
            sp <- spanFrom start
            pure $ ReturnStmt sp vals

pDeferStmt :: Parser (Stmt SrcSpan)
pDeferStmt = do
    start <- getPos
    keyword "defer"
    s <- pStmt
    sp <- spanFrom start
    pure $ DeferStmt sp s

pBranchStmt :: Parser (Stmt SrcSpan)
pBranchStmt =
    choice
        [ pBranch "break" Break
        , pBranch "continue" Continue
        , do
            start <- getPos
            keyword "fallthrough"
            pure $ BranchStmt (SrcSpan start start) Fallthrough Nothing
        ]
  where
    pBranch kw kind = do
        start <- getPos
        keyword kw
        label <- optional (try pLabelIdent)
        case label of
            Nothing -> pure $ BranchStmt (SrcSpan start start) kind Nothing
            Just l -> do
                ep <- endPos
                pure $ BranchStmt (SrcSpan start ep) kind (Just l)
    -- Label must be on same line: psPendingSemi means newline already occurred
    pLabelIdent = do
        hasSemi <- psPendingSemi <$> lift get
        when hasSemi $ fail "label not on same line"
        lstart <- getPos
        name <- ident
        lend <- endPos
        pure $ Ident (SrcSpan lstart lend) name

pUsingStmt :: Parser (Stmt SrcSpan)
pUsingStmt = do
    start <- getPos
    keyword "using"
    choice
        [ try $ do
            exprs <- pExprList
            choice
                [ pDeclRest exprs
                , pAssignRest exprs
                , case exprs of
                    [e] -> pure $ UsingStmt (SrcSpan start (spanEnd (exprSpan e))) [e]
                    _ -> do sp <- spanFrom start; pure $ UsingStmt sp exprs
                ]
        ]

pExprList :: Parser [Expr SrcSpan]
pExprList = pExpr `sepBy1` comma

pExpr :: Parser (Expr SrcSpan)
pExpr = pExpr' True

pExprNoCompLit :: Parser (Expr SrcSpan)
pExprNoCompLit = pExpr' False

pExpr' :: Bool -> Parser (Expr SrcSpan)
pExpr' acl = do
    e <- pOrElseExpr' acl
    pTernary e
  where
    -- psPendingSemi=True means newline was seen: next if/when is a new statement, not ternary
    pTernary e = do
        st <- lift get
        if psPendingSemi st
            then
                choice
                    [ try $ do
                        op "?"
                        then_ <- pOrElseExpr' acl
                        colon
                        els <- pExpr' acl
                        pure $ TernaryIfExpr (exprSpan e <-> exprSpan els) e then_ els
                    , pure e
                    ]
            else
                choice
                    [ try $ do
                        keyword "if"
                        cond <- pOrElseExpr' False
                        keyword "else"
                        els <- pExpr' acl
                        pure $ TernaryIfExpr (exprSpan e <-> exprSpan els) cond e els
                    , try $ do
                        keyword "when"
                        cond <- pOrElseExpr' False
                        keyword "else"
                        els <- pExpr' acl
                        pure $ TernaryWhenExpr (exprSpan e <-> exprSpan els) cond e els
                    , try $ do
                        op "?"
                        then_ <- pOrElseExpr' acl
                        colon
                        els <- pExpr' acl
                        pure $ TernaryIfExpr (exprSpan e <-> exprSpan els) e then_ els
                    , pure e
                    ]

pOrElseExpr' :: Bool -> Parser (Expr SrcSpan)
pOrElseExpr' acl = do
    e <- pLogOrExpr' acl
    pOrSuffix e
  where
    pOrSuffix e =
        choice
            [ try $ do
                keyword "or_else"
                r <- pLogOrExpr' True
                pOrSuffix (OrElseExpr (exprSpan e <-> exprSpan r) e r)
            , pure e
            ]

pLogOrExpr' :: Bool -> Parser (Expr SrcSpan)
pLogOrExpr' acl =
    pLeftAssoc
        (pLogAndExpr' acl)
        (pLogAndExprRhs acl)
        [(OpLogOr, op "||")]

pLogAndExpr' :: Bool -> Parser (Expr SrcSpan)
pLogAndExpr' acl =
    pLeftAssoc
        (pCompareExpr' acl)
        (pCompareExprRhs acl)
        [(OpLogAnd, op "&&")]

pLogAndExprRhs, pCompareExprRhs, pInExprRhs, pAddExprRhs, pMulExprRhs :: Bool -> Parser (Expr SrcSpan)
pLogAndExprRhs acl = pLeftAssoc (pCompareExprRhs acl) (pCompareExprRhs acl) [(OpLogAnd, op "&&")]
pCompareExprRhs acl =
    pLeftAssoc
        (pInExprRhs acl)
        (pInExprRhs acl)
        [ (OpEq, try $ op "==")
        , (OpNotEq, try $ op "!=")
        , (OpLtEq, try $ op "<=")
        , (OpGtEq, try $ op ">=")
        , (OpLt, try $ op "<")
        , (OpGt, try $ op ">")
        , (OpRangeFull, try $ op "..=")
        , (OpRangeHalf, try $ op "..<")
        ]
pInExprRhs acl =
    pLeftAssoc
        (pAddExprRhs acl)
        (pAddExprRhs acl)
        [(OpIn, try $ keyword "in"), (OpNotIn, try $ keyword "not_in")]
pAddExprRhs acl =
    pLeftAssoc
        (pMulExprRhs acl)
        (pMulExprRhs acl)
        [(OpAdd, try $ op "+"), (OpSub, try $ op "-"), (OpBitOr, try $ op "|"), (OpBitXor, try $ op "~")]
pMulExprRhs acl =
    pLeftAssoc
        (pUnaryExprImpl True acl)
        (pUnaryExprImpl True acl)
        [ (OpMul, try $ op "*")
        , (OpQuo, try $ op "/")
        , (OpModMod, try $ op "%%")
        , (OpMod, try $ op "%")
        , (OpBitAnd, try $ op "&")
        , (OpShl, try $ op "<<")
        , (OpShr, try $ op ">>")
        , (OpBitAndNot, try $ op "&~")
        ]

pCompareExpr' :: Bool -> Parser (Expr SrcSpan)
pCompareExpr' acl =
    pLeftAssoc
        (pInExpr' acl)
        (pInExprRhs acl)
        [ (OpEq, try $ op "==")
        , (OpNotEq, try $ op "!=")
        , (OpLtEq, try $ op "<=")
        , (OpGtEq, try $ op ">=")
        , (OpLt, try $ op "<")
        , (OpGt, try $ op ">")
        , (OpRangeFull, try $ op "..=")
        , (OpRangeHalf, try $ op "..<")
        ]

pInExpr' :: Bool -> Parser (Expr SrcSpan)
pInExpr' acl =
    pLeftAssoc
        (pAddExpr' acl)
        (pAddExprRhs acl)
        [ (OpIn, try $ keyword "in")
        , (OpNotIn, try $ keyword "not_in")
        ]

pAddExpr' :: Bool -> Parser (Expr SrcSpan)
pAddExpr' acl =
    pLeftAssoc
        (pMulExpr' acl)
        (pMulExprRhs acl)
        [ (OpAdd, try $ op "+")
        , (OpSub, try $ op "-")
        , (OpBitOr, try $ op "|")
        , (OpBitXor, try $ op "~")
        ]

pMulExpr' :: Bool -> Parser (Expr SrcSpan)
pMulExpr' acl =
    pLeftAssoc
        (pUnaryExpr' acl)
        (pUnaryExprImpl True acl)
        [ (OpMul, try $ op "*")
        , (OpQuo, try $ op "/")
        , (OpModMod, try $ op "%%")
        , (OpMod, try $ op "%")
        , (OpBitAnd, try $ op "&")
        , (OpShl, try $ op "<<")
        , (OpShr, try $ op ">>")
        , (OpBitAndNot, try $ op "&~")
        ]

pLeftAssoc :: Parser (Expr SrcSpan) -> Parser (Expr SrcSpan) -> [(BinOp, Parser ())] -> Parser (Expr SrcSpan)
pLeftAssoc pLHS pRHS ops = do
    l <- pLHS
    go l
  where
    go l = do
        result <- optional (choice [try (bop <$ p) | (bop, p) <- ops])
        case result of
            Nothing -> pure l
            Just bop -> do
                r <- pRHS
                go (BinaryExpr (exprSpan l <-> exprSpan r) l bop r)

pUnaryExpr :: Parser (Expr SrcSpan)
pUnaryExpr = pUnaryExpr' True

pUnaryExprNoCompLit :: Parser (Expr SrcSpan)
pUnaryExprNoCompLit = pUnaryExpr' False

pUnaryExpr' :: Bool -> Parser (Expr SrcSpan)
pUnaryExpr' acl = pUnaryExprImpl acl acl

pUnaryExprImpl :: Bool -> Bool -> Parser (Expr SrcSpan)
pUnaryExprImpl untypedCL typedCL =
    choice
        [ pUnary "+" OpPos
        , pUnary "-" OpNeg
        , pUnary "!" OpNot
        , try (pUnary "~" OpBitNot)
        , try (pUnary "&" OpAddr)
        , pAutoCast
        , pCastExpr
        , pTransmuteExpr
        , pPostfixExpr untypedCL typedCL
        ]
  where
    pUnary s uop = do
        start <- getPos
        op s
        e <- pUnaryExprImpl untypedCL typedCL
        let sp = SrcSpan start (spanEnd (exprSpan e))
        pure $ UnaryExpr sp uop (Just e)
    pAutoCast = do
        start <- getPos
        keyword "auto_cast"
        e <- pUnaryExprImpl untypedCL typedCL
        let sp = SrcSpan start (spanEnd (exprSpan e))
        pure $ AutoCast sp e
    pCastExpr = do
        start <- getPos
        keyword "cast"
        lparen
        ty <- pExpr
        rparen
        e <- pUnaryExprImpl untypedCL typedCL
        let sp = SrcSpan start (spanEnd (exprSpan e))
        pure $ TypeCast sp CastNormal ty e
    pTransmuteExpr = do
        start <- getPos
        keyword "transmute"
        lparen
        ty <- pExpr
        rparen
        e <- pUnaryExprImpl untypedCL typedCL
        let sp = SrcSpan start (spanEnd (exprSpan e))
        pure $ TypeCast sp Transmute ty e

pPostfixExpr :: Bool -> Bool -> Parser (Expr SrcSpan)
pPostfixExpr untypedCL typedCL = do
    e <- pPrimaryExpr untypedCL typedCL
    pPostfixChain typedCL e

pPostfixChain :: Bool -> Expr SrcSpan -> Parser (Expr SrcSpan)
pPostfixChain allowCompLit e =
    choice
        [ try $ do
            op "->"
            selStart <- getPos
            name <- ident
            ep <- endPos
            let selExpr = SelectorExpr (exprSpan e <-> SrcSpan ep ep) e (Ident (SrcSpan selStart ep) name)
            lparen
            (args, hasEllipsis) <- pCallArgs
            rparen
            ep' <- endPos
            let sp = exprSpan e <-> SrcSpan ep' ep'
            pPostfixChain allowCompLit (CallExpr sp selExpr args hasEllipsis)
        , try $ do
            dot
            selStart <- getPos
            name <- ident
            ep <- endPos
            let sp = exprSpan e <-> SrcSpan ep ep
            pPostfixChain allowCompLit (SelectorExpr sp e (Ident (SrcSpan selStart ep) name))
        , try $ do
            dot
            qStart <- getPos
            op "?"
            qEnd <- endPos
            ep <- endPos
            let qSpan = SrcSpan qStart qEnd
            let sp = exprSpan e <-> SrcSpan ep ep
            pPostfixChain allowCompLit (TypeAssertion sp e (UnaryExpr qSpan OpQuestion Nothing))
        , try $ do
            dot
            lparen
            ty <- pExpr
            rparen
            ep <- endPos
            let sp = exprSpan e <-> SrcSpan ep ep
            pPostfixChain allowCompLit (TypeAssertion sp e ty)
        , try $ do
            st <- lift get
            guard (not $ psPendingSemi st)
            lbracket
            idx <- optional pExpr
            mCol <- optional (comma *> pExpr)
            result <- optional (colon *> optional pExpr)
            rbracket
            ep <- endPos
            let sp = exprSpan e <-> SrcSpan ep ep
            case mCol of
                Just col -> case idx of
                    Just row -> pPostfixChain allowCompLit (MatrixIndexExpr sp e row col)
                    Nothing -> fail "empty matrix row index"
                Nothing -> case result of
                    Nothing -> case idx of
                        Just i -> pPostfixChain allowCompLit (IndexExpr sp e i)
                        Nothing -> fail "empty index"
                    Just hi -> pPostfixChain allowCompLit (SliceExpr sp e idx hi)
        , try $ do
            st <- lift get
            guard (not $ psPendingSemi st)
            lparen
            (args, hasEllipsis) <- pCallArgs
            rparen
            ep <- endPos
            let sp = exprSpan e <-> SrcSpan ep ep
            pPostfixChain allowCompLit (CallExpr sp e args hasEllipsis)
        , try $ do
            op "^"
            ep <- endPos
            let sp = exprSpan e <-> SrcSpan ep ep
            pPostfixChain allowCompLit (DerefExpr sp e)
        , do
            keyword "or_return"
            sp <- spanFrom (spanStart (exprSpan e))
            pPostfixChain allowCompLit (OrReturnExpr sp e)
        , do
            keyword "or_break"
            lbl <- optional $ try $ do
                st' <- lift get
                guard (not (psPendingSemi st'))
                s <- getPos
                n <- ident
                e' <- endPos
                pure $ Ident (SrcSpan s e') n
            sp <- spanFrom (spanStart (exprSpan e))
            pPostfixChain allowCompLit (OrBranchExpr sp e Break lbl)
        , do
            keyword "or_continue"
            lbl <- optional $ try $ do
                st' <- lift get
                guard (not (psPendingSemi st'))
                s <- getPos
                n <- ident
                e' <- endPos
                pure $ Ident (SrcSpan s e') n
            sp <- spanFrom (spanStart (exprSpan e))
            pPostfixChain allowCompLit (OrBranchExpr sp e Continue lbl)
        , -- Full PState save/restore needed: Megaparsec's try doesn't revert monadic State
          if allowCompLit
            then do
                savedSt <- lift get
                result <- optional $ try $ do
                    lift $ modify $ \s -> s{psPendingSemi = False}
                    lbrace
                    elems <- option [] pCompElems
                    _ <- optional comma
                    rbrace
                    ep <- endPos
                    let sp = exprSpan e <-> SrcSpan ep ep
                    pure (CompLit sp (Just e) elems)
                case result of
                    Just e' -> pPostfixChain allowCompLit e'
                    Nothing -> do
                        lift $ put savedSt
                        pure e
            else pure e
        ]

pCallArgs :: Parser ([Expr SrcSpan], Bool)
pCallArgs = do
    args <- option [] (pCallArg `sepBy1` comma)
    _ <- optional comma
    let (args', hasEllipsis) = unwrapEllipsis args
    pure (args', hasEllipsis)
  where
    unwrapEllipsis [] = ([], False)
    unwrapEllipsis xs = go [] xs
    go acc [] = (reverse acc, False)
    go acc (Ellipsis _ (Just e) : rest) = (reverse acc ++ [e] ++ rest, True)
    go acc (x : rest) = go (x : acc) rest

pCallArg :: Parser (Expr SrcSpan)
pCallArg = try pFieldValueArg <|> pExpr
  where
    pFieldValueArg = do
        key <- pExpr
        op "="
        val <- pExpr
        pure $ FieldValue (exprSpan key <-> exprSpan val) key val

pCompElems :: Parser [Expr SrcSpan]
pCompElems = pCompElem `sepBy1` comma

pCompElem :: Parser (Expr SrcSpan)
pCompElem = try pFieldValueElem <|> pExpr
  where
    pFieldValueElem = do
        key <- pExpr
        op "="
        val <- pExpr
        pure $ FieldValue (exprSpan key <-> exprSpan val) key val

pPrimaryExpr :: Bool -> Bool -> Parser (Expr SrcSpan)
pPrimaryExpr untypedCL typedCL =
    choice
        ( [ pIdentExpr
          , pIntLit
          , pFloatLit
          , pImagLit
          , pRuneLit
          , pStringLit
          , pParenExpr
          , pUndefExpr
          , pDirectiveExpr untypedCL typedCL
          , pImplicitSelector
          , pContextExpr
          , tryState pProcLit
          , tryState pProcGroup
          ]
            ++
            -- When untypedCL=False: still parse empty `{}` since `{`+`}` cannot be a block
            -- in expression position (e.g. `if y & z == {} {`)
            ( if untypedCL
                then [try pUntypedCompLit]
                else [try pEmptyCompLit]
            )
            ++ [ pTypeExpr
               , pEllipsis
               ]
        )
        <?> "expression"

pUntypedCompLit :: Parser (Expr SrcSpan)
pUntypedCompLit = do
    start <- getPos
    lbrace
    elems <- option [] pCompElems
    _ <- optional comma
    rbrace
    sp <- spanFrom start
    pure $ CompLit sp Nothing elems

pEmptyCompLit :: Parser (Expr SrcSpan)
pEmptyCompLit = do
    start <- getPos
    lbrace
    clearPendingSemi
    rbrace
    sp <- spanFrom start
    pure $ CompLit sp Nothing []

pIdentExpr :: Parser (Expr SrcSpan)
pIdentExpr = do
    start <- getPos
    name <- ident
    sp <- spanFrom start
    pure $ Ident sp name

pIntLit :: Parser (Expr SrcSpan)
pIntLit = do
    start <- getPos
    (txt, _) <- pInteger
    sp <- spanFrom start
    pure $ BasicLit sp LitInt txt

pFloatLit :: Parser (Expr SrcSpan)
pFloatLit = do
    start <- getPos
    (txt, _) <- pFloat
    sp <- spanFrom start
    pure $ BasicLit sp LitFloat txt

pImagLit :: Parser (Expr SrcSpan)
pImagLit = do
    start <- getPos
    (txt, _) <- pImag
    sp <- spanFrom start
    pure $ BasicLit sp LitImag txt

pRuneLit :: Parser (Expr SrcSpan)
pRuneLit = do
    start <- getPos
    raw <- pRuneRaw
    sp <- spanFrom start
    pure $ BasicLit sp LitRune raw

pStringLit :: Parser (Expr SrcSpan)
pStringLit = do
    start <- getPos
    raw <- pStringRaw
    sp <- spanFrom start
    pure $ BasicLit sp LitString raw

pParenExpr :: Parser (Expr SrcSpan)
pParenExpr = do
    start <- getPos
    lparen
    e <- pExpr
    rparen
    sp <- spanFrom start
    pure $ ParenExpr sp e

pUndefExpr :: Parser (Expr SrcSpan)
pUndefExpr = do
    start <- getPos
    op "---"
    sp <- spanFrom start
    pure $ Undef sp

pDirectiveExpr :: Bool -> Bool -> Parser (Expr SrcSpan)
pDirectiveExpr untypedCL typedCL = do
    start <- getPos
    op "#"
    name <- ident
    case name of
        "type" -> do
            ty <- pUnaryExpr' True
            sp <- spanFrom start
            pure $ HelperType sp ty
        "force_inline" -> pProcLitAfterDirective start <|> pUnaryExprImpl untypedCL typedCL
        "force_no_inline" -> pProcLitAfterDirective start <|> pUnaryExprImpl untypedCL typedCL
        "simd" -> pTaggedType start name
        "soa" -> pTaggedType start name
        "sparse" -> pTaggedType start name
        "row_major" -> pMatrixTagExpr start name
        "column_major" -> pMatrixTagExpr start name
        "relative" -> pRelativeType start name
        _ -> do
            nameEnd <- endPos
            let sp = SrcSpan start nameEnd
            pure $ BasicDirective sp name Nothing
  where
    pTaggedType _dirStart _name = do
        choice
            [ pArrayOrSliceType' Nothing
            , do
                pStart <- getPos
                op "^"
                e <- pUnaryExprNoCompLit
                let sp = SrcSpan pStart (spanEnd (exprSpan e))
                pure $ PointerType sp e Nothing
            ]
    -- Odin's parser sets Tag_Expr end to inner expression's start position
    pMatrixTagExpr dirStart tagName = do
        matTy <- pUnaryExprNoCompLit
        let tagSp = SrcSpan dirStart (spanStart (exprSpan matTy))
        mBody <- optional $ try $ do
            lbrace
            elems <- option [] pCompElems
            _ <- optional comma
            rbrace
            pure elems
        case mBody of
            Nothing -> pure $ TagExpr tagSp tagName matTy
            Just elems -> do
                ep <- endPos
                let litSp = SrcSpan (spanStart (exprSpan matTy)) ep
                pure $ TagExpr tagSp tagName (CompLit litSp (Just matTy) elems)

    pRelativeType dirStart dirName = do
        nameEnd <- endPos
        let dirSp = SrcSpan dirStart nameEnd
        let dir = BasicDirective dirSp dirName Nothing
        lparen
        (args, hasEllipsis) <- pCallArgs
        rparen
        callEnd <- endPos
        let callSp = SrcSpan dirStart callEnd
        let tag = CallExpr callSp dir args hasEllipsis
        ty <- pUnaryExprNoCompLit
        sp <- spanFrom dirStart
        pure $ RelativeType sp tag ty

pImplicitSelector :: Parser (Expr SrcSpan)
pImplicitSelector = try $ do
    start <- getPos
    dot
    nameStart <- getPos
    name <- ident
    nameEnd <- endPos
    sp <- spanFrom start
    pure $ ImplicitSelectorExpr sp (Ident (SrcSpan nameStart nameEnd) name)

pContextExpr :: Parser (Expr SrcSpan)
pContextExpr = do
    start <- getPos
    keyword "context"
    sp <- spanFrom start
    pure $ Implicit sp

pProcLit :: Parser (Expr SrcSpan)
pProcLit = do
    start <- getPos
    _ <- optional $ try $ do op "#"; ident
    pProcLitAfterDirective start

pProcLitAfterDirective :: SrcPos -> Parser (Expr SrcSpan)
pProcLitAfterDirective _directiveStart = do
    ty <- pProcType
    let procStart = spanStart (exprSpan ty)
    _ <- many $ try $ do
        clearPendingSemi
        op "#"
        ident
    body <-
        choice
            [ Just <$> pBlock
            , do
                op "---"
                pure Nothing
            ]
    sp <- spanFrom procStart
    pure $ ProcLit sp ty body

pProcGroup :: Parser (Expr SrcSpan)
pProcGroup = do
    start <- getPos
    keyword "proc"
    lbrace
    exprs <- pExpr `sepBy1` comma
    _ <- optional comma
    rbrace
    sp <- spanFrom start
    pure $ ProcGroup sp exprs

pEllipsis :: Parser (Expr SrcSpan)
pEllipsis = do
    op ".."
    _ <- optional (try (op "."))
    exprStart <- getPos
    e <- optional (try pExpr)
    sp <- spanFrom exprStart
    pure $ Ellipsis sp e

pTypeExpr :: Parser (Expr SrcSpan)
pTypeExpr =
    choice
        [ pPointerType
        , pArrayOrSliceType
        , pMapType
        , pStructType
        , pUnionType
        , pEnumType
        , pBitSetType
        , pBitFieldType
        , pProcTypeExpr
        , pMatrixType
        , pDistinctType
        , pPolyType
        , pTypeidType
        ]
        <?> "type expression"

pPointerType :: Parser (Expr SrcSpan)
pPointerType = do
    start <- getPos
    op "^"
    e <- pUnaryExprNoCompLit
    let sp = SrcSpan start (spanEnd (exprSpan e))
    pure $ PointerType sp e Nothing

pArrayOrSliceType :: Parser (Expr SrcSpan)
pArrayOrSliceType = pArrayOrSliceType' Nothing

pArrayOrSliceType' :: Maybe (Expr SrcSpan) -> Parser (Expr SrcSpan)
pArrayOrSliceType' tag = do
    start <- getPos
    lbracket
    choice
        [ try $ do
            op "^"
            rbracket
            e <- pUnaryExprNoCompLit
            let sp = SrcSpan start (spanEnd (exprSpan e))
            pure $ MultiPointerType sp e
        , try $ do
            keyword "dynamic"
            rbracket
            e <- pUnaryExprNoCompLit
            let sp = SrcSpan start (spanEnd (exprSpan e))
            pure $ DynamicArrayType sp e tag
        , try $ do
            qStart <- getPos
            op "?"
            qEnd <- endPos
            rbracket
            e <- pUnaryExprNoCompLit
            let sp = SrcSpan start (spanEnd (exprSpan e))
            let autoLen = UnaryExpr (SrcSpan qStart qEnd) OpQuestion Nothing
            pure $ ArrayType sp (Just autoLen) e tag
        , try $ do
            rbracket
            e <- pUnaryExprNoCompLit
            let sp = SrcSpan start (spanEnd (exprSpan e))
            pure $ ArrayType sp Nothing e tag
        , do
            len <- pExpr
            rbracket
            e <- pUnaryExprNoCompLit
            let sp = SrcSpan start (spanEnd (exprSpan e))
            pure $ ArrayType sp (Just len) e tag
        ]

pMapType :: Parser (Expr SrcSpan)
pMapType = do
    start <- getPos
    keyword "map"
    lbracket
    key <- pExpr
    rbracket
    val <- pUnaryExprNoCompLit
    let sp = SrcSpan start (spanEnd (exprSpan val))
    pure $ MapType sp key val

pStructType :: Parser (Expr SrcSpan)
pStructType = do
    start <- getPos
    keyword "struct"
    (flags1, align1) <- pStructDirectives
    mParams <- optional $ try $ do
        lparen
        paramStart <- getPos
        ps <- map fixParamTypeid <$> (pParam `sepBy1` comma)
        _ <- optional comma
        paramEnd <- getPos
        rparen
        let ps' = map addUnderscoreName ps
        pure $ FieldList (SrcSpan paramStart paramEnd) ps'
    (flags2, align2) <- pStructDirectives
    -- Clear pending semi since `where` can be on next line after `)` or directives
    _where <- optional $ try $ do
        clearPendingSemi
        keyword "where"
        void $ pExprNoCompLit `sepBy1` comma
        pure ()
    clearPendingSemi
    lbrace
    flStart <- getPos
    fields <- option [] (pStructField `sepEndBy1` comma)
    clearPendingSemi
    flEnd <- getPos
    rbrace
    sp <- spanFrom start
    let fl = FieldList (SrcSpan flStart flEnd) fields
    let align = align1 <|> align2
    pure $ StructType sp mParams fl align (flags1 ++ flags2)

pStructDirectives :: Parser ([StructFlag], Maybe (Expr SrcSpan))
pStructDirectives = do
    items <- many pStructDirective
    let flags = [f | Left f <- items]
    let aligns = [a | Right (Just a) <- items]
    pure (flags, case aligns of (a : _) -> Just a; _ -> Nothing)

pStructDirective :: Parser (Either StructFlag (Maybe (Expr SrcSpan)))
pStructDirective =
    choice
        [ Left StructPacked <$ try (pHashIdent "packed")
        , Left StructRawUnion <$ try (pHashIdent "raw_union")
        , try $ do void (pHashIdent "align"); e <- pAlignExpr; pure (Right (Just e))
        , -- Odin AST has field_align for these; our AST doesn't, so parse but discard
          try $ do void (pHashIdent "max_field_align"); void pAlignExpr; pure (Right Nothing)
        , try $ do void (pHashIdent "min_field_align"); void pAlignExpr; pure (Right Nothing)
        ]
  where
    pAlignExpr =
        choice
            [ do
                pStart <- getPos
                lparen
                e <- pExpr
                rparen
                sp <- spanFrom pStart
                pure $ ParenExpr sp e
            , pUnaryExprNoCompLit
            ]

pStructField :: Parser (Field SrcSpan)
pStructField = do
    flags <- pFieldFlags
    nameStart <- getPos
    names <- (try pFieldName) `sepBy1` comma
    colon
    ty <- pExpr
    dflt <- optional (try (op "=" *> pExpr))
    let sp = fieldSpan nameStart (Just ty) dflt
    tag <- optional (try pStringRaw)
    pure $ Field sp names (Just ty) dflt tag flags

pFieldFlags :: Parser [FieldFlag]
pFieldFlags =
    many $
        choice
            [ FieldUsing <$ try (keyword "using")
            , FieldAnyInt <$ try (pHashIdent "any_int")
            , FieldCVararg <$ try (pHashIdent "c_vararg")
            , FieldNoAlias <$ try (pHashIdent "no_alias")
            , FieldSubtype <$ try (pHashIdent "subtype")
            , FieldConst <$ try (pHashIdent "const")
            , FieldByPtr <$ try (pHashIdent "by_ptr")
            , FieldNoBroadcast <$ try (pHashIdent "no_broadcast")
            ]

pFieldName :: Parser (Expr SrcSpan)
pFieldName = do
    start <- getPos
    name <- identOrKeyword
    sp <- spanFrom start
    pure $ Ident sp name

pUnionType :: Parser (Expr SrcSpan)
pUnionType = do
    start <- getPos
    keyword "union"
    flags <- many pUnionFlag
    mParams <- optional $ try $ do
        lparen
        paramStart <- getPos
        ps <- map fixParamTypeid <$> (pParam `sepBy1` comma)
        _ <- optional comma
        paramEnd <- getPos
        rparen
        let ps' = map addUnderscoreName ps
        pure $ FieldList (SrcSpan paramStart paramEnd) ps'
    _where <- optional $ try $ do
        keyword "where"
        void $ pExprNoCompLit `sepBy1` comma
        pure ()
    clearPendingSemi
    lbrace
    variants <- option [] (pExpr `sepEndBy1` comma)
    clearPendingSemi
    rbrace
    sp <- spanFrom start
    pure $ UnionType sp variants mParams flags

pUnionFlag :: Parser UnionFlag
pUnionFlag =
    choice
        [ UnionNoNil <$ try (pHashIdent "no_nil")
        , UnionSharedNil <$ try (pHashIdent "shared_nil")
        ]

pEnumType :: Parser (Expr SrcSpan)
pEnumType = do
    start <- getPos
    keyword "enum"
    backing <- optional $ try $ do
        MP.notFollowedBy (void $ MP.single '{')
        pUnaryExprNoCompLit
    clearPendingSemi
    lbrace
    fields <- option [] (pEnumField `sepEndBy1` comma)
    clearPendingSemi
    rbrace
    sp <- spanFrom start
    pure $ EnumType sp backing fields

pEnumField :: Parser (Expr SrcSpan)
pEnumField = try pEnumFieldWithValue <|> pFieldName
  where
    pEnumFieldWithValue = do
        name <- pFieldName
        op "="
        val <- pExpr
        pure $ FieldValue (exprSpan name <-> exprSpan val) name val

pBitSetType :: Parser (Expr SrcSpan)
pBitSetType = do
    start <- getPos
    keyword "bit_set"
    lbracket
    elem_ <- pExpr
    underlying <- optional (semi *> pExpr)
    rbracket
    sp <- spanFrom start
    pure $ BitSetType sp elem_ underlying

pBitFieldType :: Parser (Expr SrcSpan)
pBitFieldType = do
    start <- getPos
    keyword "bit_field"
    backing <- pUnaryExprNoCompLit
    clearPendingSemi
    lbrace
    fields <- option [] (pBitFieldField `sepEndBy1` comma)
    clearPendingSemi
    rbrace
    sp <- spanFrom start
    pure $ BitFieldType sp (Just backing) fields

pBitFieldField :: Parser (BitFieldField SrcSpan)
pBitFieldField = do
    start <- getPos
    -- Odin parser bug: parse_ident() produces Ident{name="_"} for keywords as
    -- bit_field field names. We match this.
    name <- pBitFieldFieldName
    colon
    ty <- pUnaryExpr -- restricted: no binary ops, since `|` separates type from size
    op "|"
    size <- pExpr
    sp <- spanFrom start
    pure $ BitFieldField sp name ty size

pBitFieldFieldName :: Parser (Expr SrcSpan)
pBitFieldFieldName = pIdentOnly <|> pKeywordAsUnderscore
  where
    pIdentOnly = do
        start <- getPos
        name <- ident
        sp <- spanFrom start
        pure $ Ident sp name
    pKeywordAsUnderscore = do
        start <- getPos
        _ <- identOrKeyword
        sp <- spanFrom start
        pure $ Ident sp "_"

pProcType :: Parser (Expr SrcSpan)
pProcType = do
    start <- getPos
    keyword "proc"
    ccVal <- optional (try pString)
    lparen
    paramStart <- getPos
    params <- option [] (map fixParamTypeid <$> (pParam `sepBy1` comma))
    _ <- optional comma
    paramEnd <- getPos
    rparen
    rparenEnd <- endPos
    (results, resultsEnd) <- do
        mr <- optional $ try $ do
            op "->"
            choice
                [ try $ do
                    lnBefore <- MP.sourceLine <$> MP.getSourcePos
                    op "!"
                    lnAfter <- MP.sourceLine <$> MP.getSourcePos
                    ep' <- endPos
                    -- op "!" uses tok False; manually set auto-semi since ! is terminal
                    lift $ modify $ \s -> s{psAutoSemi = True}
                    when (lnAfter > lnBefore) $
                        lift $
                            modify $ \s ->
                                s
                                    { psPendingSemi = True
                                    , psSemiEndPos = ep'{posOffset = posOffset ep' + 1, posCol = posCol ep' + 1}
                                    }
                    pure (Nothing, ep')
                , try $ do
                    lparen
                    resStart <- getPos
                    rs <- pParam `sepBy1` comma
                    _ <- optional comma
                    resEnd <- getPos
                    rparen
                    resRparenEnd <- endPos
                    let rs' = map addUnderscoreName rs
                    pure (Just (FieldList (SrcSpan resStart resEnd) rs'), resRparenEnd)
                , do
                    r <- pResultType
                    let rsp = exprSpan r
                    pure (Just (FieldList rsp [Field rsp [] (Just r) Nothing Nothing []]), spanEnd rsp)
                ]
        pure $ case mr of
            Just (mfl, ep) -> (mfl, ep)
            Nothing -> (Nothing, rparenEnd)
    -- Save/restore semi state: clearPendingSemi must not persist if where fails
    savedPending <- psPendingSemi <$> lift get
    savedAuto <- psAutoSemi <$> lift get
    _where <- optional $ try $ do
        clearPendingSemi
        keyword "where"
        pExprNoCompLit `sepBy1` comma
    case _where of
        Nothing -> lift $ modify $ \s -> s{psPendingSemi = savedPending, psAutoSemi = savedAuto}
        Just _ -> pure ()
    let sp = SrcSpan start resultsEnd
    let params' = map addUnderscoreName params
    let paramList = FieldList (SrcSpan paramStart paramEnd) params'
    pure $ ProcType sp paramList results ccVal

-- pProcType must come first so `proc "c" (int)` is parsed as ProcType, not ProcLit
pResultType :: Parser (Expr SrcSpan)
pResultType =
    choice
        [ tryState pProcType
        , pUnaryExprNoCompLit
        ]

addUnderscoreName :: Field SrcSpan -> Field SrcSpan
addUnderscoreName f@(Field sp names ty dflt tag flags)
    | null names
    , Just tyExpr <- ty =
        let tsp = exprSpan tyExpr
            syntheticName = Ident (SrcSpan (spanStart tsp) (SrcPos (posOffset (spanStart tsp) + 1) (posLine (spanStart tsp)) (posCol (spanStart tsp) + 1))) "_"
         in Field sp [syntheticName] ty dflt tag flags
    | otherwise = f

pProcTypeExpr :: Parser (Expr SrcSpan)
pProcTypeExpr = tryState $ do
    _ <- optional $ try $ do op "#"; ident
    ty <- pProcType
    MP.notFollowedBy (void $ MP.single '{')
    MP.notFollowedBy (void $ MP.chunk "---")
    pure ty

pParam :: Parser (Field SrcSpan)
pParam =
    choice
        [ try $ do
            keyword "using"
            nameStart <- getPos
            name <- ident
            nameEnd <- endPos
            colon
            ty <- pExpr
            dflt <- optional (try (op "=" *> pExpr))
            let sp = fieldSpan nameStart (Just ty) dflt
            pure $ Field sp [Ident (SrcSpan nameStart nameEnd) name] (Just ty) dflt Nothing [FieldUsing]
        , try $ do
            flags <- pFieldFlags
            guard (not (null flags))
            nameStart <- getPos
            firstName <- ident
            firstEnd <- endPos
            restParts <- many $ try $ do
                comma
                _ <- optional (try pFieldFlags)
                s <- getPos
                n <- ident
                e <- endPos
                pure (n, s, e)
            colon
            let names = Ident (SrcSpan nameStart firstEnd) firstName : [Ident (SrcSpan s e) n | (n, s, e) <- restParts]
            inferredDefault <- optional (try (op "="))
            case inferredDefault of
                Just _ -> do
                    dflt <- pExpr
                    let sp = fieldSpan nameStart Nothing (Just dflt)
                    pure $ Field sp names Nothing (Just dflt) Nothing flags
                Nothing -> do
                    ty <- pExpr
                    dflt <- optional (try (op "=" *> pExpr))
                    let sp = fieldSpan nameStart (Just ty) dflt
                    pure $ Field sp names (Just ty) dflt Nothing flags
        , try $ do
            start <- getPos
            let pPolyName = do
                    s <- getPos
                    op "$"
                    ns <- getPos
                    n <- ident
                    ne <- endPos
                    pure $ PolyType (SrcSpan s ne) (Ident (SrcSpan ns ne) n) Nothing
            firstName <- pPolyName
            restNames <- many $ try (comma *> pPolyName)
            colon
            ty <- pExpr
            dflt <- optional (try (op "=" *> pExpr))
            let sp = fieldSpan start (Just ty) dflt
            pure $ Field sp (firstName : restNames) (Just ty) dflt Nothing []
        , try $ do
            start <- getPos
            firstName <- ident
            firstEnd <- endPos
            _ <- comma
            restParts <- pNameWithEnd `sepBy1` comma
            colon
            ty <- pExpr
            dflt <- optional (try (op "=" *> pExpr))
            let sp = fieldSpan start (Just ty) dflt
            let names = Ident (SrcSpan start firstEnd) firstName : [Ident (SrcSpan s e) n | (n, s, e) <- restParts]
            pure $ Field sp names (Just ty) dflt Nothing []
        , try $ do
            start <- getPos
            name <- ident
            nameEnd <- endPos
            colon
            ty <- pExpr
            dflt <- optional (try (op "=" *> pExpr))
            let sp = fieldSpan start (Just ty) dflt
            pure $ Field sp [Ident (SrcSpan start nameEnd) name] (Just ty) dflt Nothing []
        , try $ do
            start <- getPos
            op "$"
            nameStart <- getPos
            name <- ident
            nameEnd <- endPos
            colon
            op "="
            dflt <- pExpr
            let sp = fieldSpan start Nothing (Just dflt)
            let nameExpr = PolyType (SrcSpan start nameEnd) (Ident (SrcSpan nameStart nameEnd) name) Nothing
            pure $ Field sp [nameExpr] Nothing (Just dflt) Nothing []
        , try $ do
            start <- getPos
            name <- ident
            nameEnd <- endPos
            colon
            op "="
            dflt <- pExpr
            let sp = fieldSpan start Nothing (Just dflt)
            pure $ Field sp [Ident (SrcSpan start nameEnd) name] Nothing (Just dflt) Nothing []
        , try $ do
            start <- getPos
            op ".."
            _ <- optional (try (op "."))
            mty <- optional (try (pExpr))
            ep <- endPos
            case mty of
                Just ty -> do
                    -- Odin convention: span starts at the type expression, not at `..`
                    let tsp = exprSpan ty
                    pure $ Field tsp [] (Just (Ellipsis tsp (Just ty))) Nothing Nothing []
                Nothing -> do
                    let sp = SrcSpan start ep
                    let badExpr = BadExpr sp
                    pure $ Field sp [Ident sp "_"] (Just (Ellipsis sp (Just badExpr))) Nothing Nothing []
        , do
            ty <- pExpr
            let sp = exprSpan ty
            pure $ Field sp [] (Just ty) Nothing Nothing []
        ]
  where
    pNameWithEnd = do
        s <- getPos
        n <- ident
        e <- endPos
        pure (n, s, e)

pMatrixType :: Parser (Expr SrcSpan)
pMatrixType = do
    start <- getPos
    keyword "matrix"
    lbracket
    rows <- pExpr
    comma
    cols <- pExpr
    rbracket
    elem_ <- pUnaryExprNoCompLit
    let sp = SrcSpan start (spanEnd (exprSpan elem_))
    pure $ MatrixType sp rows cols elem_

pDistinctType :: Parser (Expr SrcSpan)
pDistinctType = do
    start <- getPos
    keyword "distinct"
    e <- pUnaryExpr
    let sp = SrcSpan start (spanEnd (exprSpan e))
    pure $ DistinctType sp e

pPolyType :: Parser (Expr SrcSpan)
pPolyType = do
    start <- getPos
    op "$"
    nameStart <- getPos
    name <- ident
    nameEnd <- endPos
    spec <- optional $ try $ do
        op "/"
        pExpr
    let sp = case spec of
            Just e -> SrcSpan start (spanStart (exprSpan e))
            Nothing -> SrcSpan start nameEnd
    let nameExpr = Ident (SrcSpan nameStart nameEnd) name
    pure $ PolyType sp nameExpr spec

pTypeidType :: Parser (Expr SrcSpan)
pTypeidType = do
    start <- getPos
    keyword "typeid"
    kwEnd <- endPos
    spec <- optional $ try (op "/" *> pExpr)
    case spec of
        Nothing -> pure $ TypeidType (SrcSpan start kwEnd) Nothing
        Just _ -> do
            sp <- spanFrom start
            pure $ TypeidType sp spec

pAttribute :: Parser (Attribute SrcSpan)
pAttribute = do
    start <- getPos
    op "@"
    choice
        [ try $ do
            lparen
            elems <- option [] (pAttrElem `sepBy1` comma)
            _ <- optional comma
            rparen
            sp <- spanFrom start
            pure $ Attribute sp elems
        , do
            nameStart <- getPos
            name <- ident
            nameEnd <- endPos
            -- Odin reference: bare Attribute.end is {0,0,0}
            let zeroPos = SrcPos 0 0 0
            let sp = SrcSpan start zeroPos
            pure $ Attribute sp [Ident (SrcSpan nameStart nameEnd) name]
        ]

pAttrElem :: Parser (Expr SrcSpan)
pAttrElem = try pAttrFieldValue <|> pExpr
  where
    pAttrFieldValue = do
        start <- getPos
        name <- ident
        nameEnd <- endPos
        op "="
        val <- pExpr
        sp <- spanFrom start
        let nameExpr = Ident (SrcSpan start nameEnd) name
        pure $ FieldValue sp nameExpr val

applyAttrs :: [Attribute SrcSpan] -> Stmt SrcSpan -> Stmt SrcSpan
applyAttrs attrs (ValueDecl sp _ names ty vals mut) =
    ValueDecl sp (reverse attrs) names ty vals mut
applyAttrs _ (ForeignBlockDecl sp lib body) =
    ForeignBlockDecl sp lib body
applyAttrs _ (ForeignImportDecl sp name paths) =
    ForeignImportDecl sp name paths
applyAttrs _ s = s

some1 :: Parser a -> Parser [a]
some1 p = do
    x <- p
    xs <- many (try p)
    pure (x : xs)

listSpan :: [Expr SrcSpan] -> SrcSpan
listSpan [] = SrcSpan (SrcPos 0 0 0) (SrcPos 0 0 0)
listSpan [e] = exprSpan e
listSpan (e : es) = exprSpan e <-> exprSpan (lastOf e es)
  where
    lastOf def [] = def
    lastOf _ [x] = x
    lastOf def (_ : xs) = lastOf def xs

-- Uses tryState for PState-safe backtracking (Megaparsec's sepBy1 doesn't restore State)
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
    x <- p
    xs <- many (tryState (sep *> p))
    pure (x : xs)

sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
sepEndBy1 p sep = do
    x <- p
    xs <- many (try (sep *> p))
    _ <- optional sep
    pure (x : xs)
