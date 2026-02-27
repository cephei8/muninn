module Muninn.Parser.Lexer (
    Parser,
    PState (..),
    initialState,
    runMuninnParser,
    sc,
    semi,
    comma,
    colon,
    dot,
    clearPendingSemi,
    lparen,
    rparen,
    lbracket,
    rbracket,
    lbrace,
    rbrace,
    op,
    keyword,
    ident,
    identOrKeyword,
    pInteger,
    pFloat,
    pImag,
    pString,
    pStringRaw,
    pRuneRaw,
    pRune,
    getPos,
    endPos,
    spanFrom,
    (<->),
    tryState,
) where

import Control.Monad (unless, void, when)
import Control.Monad.State.Strict (State, evalState, get, modify, put)
import Control.Monad.Trans (lift)
import Data.Array.Unboxed (UArray, listArray, (!))
import Data.Char (chr, digitToInt, isAlpha, isDigit, isHexDigit, ord)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Muninn.Parser.AST (Comment (..), CommentGroup (..))
import Muninn.Parser.SrcLoc (SrcPos (..), SrcSpan (..))
import Text.Megaparsec hiding (State, getSourcePos)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Read (readMaybe)

type Parser = ParsecT Void Text (State PState)

data PState = PState
    { psAutoSemi :: !Bool
    , psPendingSemi :: !Bool
    , psExprLevel :: !Int
    , psEndPos :: !SrcPos
    , psSemiEndPos :: !SrcPos
    , psByteOffsets :: !(UArray Int Int)
    , psLineStarts :: !(UArray Int Int)
    , psPrevTokLine :: !Int
    , psCommentGroups :: [CommentGroup]
    , psLeadComment :: Maybe CommentGroup
    , psLineComment :: Maybe CommentGroup
    }

initialState :: Text -> PState
initialState input =
    PState
        { psAutoSemi = False
        , psPendingSemi = False
        , psExprLevel = 0
        , psEndPos = SrcPos 0 0 0
        , psSemiEndPos = SrcPos 0 0 0
        , psByteOffsets = buildByteOffsets input
        , psLineStarts = buildLineStarts input
        , psPrevTokLine = 0
        , psCommentGroups = []
        , psLeadComment = Nothing
        , psLineComment = Nothing
        }

utf8Len :: Char -> Int
utf8Len c
    | ord c <= 0x7F = 1
    | ord c <= 0x7FF = 2
    | ord c <= 0xFFFF = 3
    | otherwise = 4

buildByteOffsets :: Text -> UArray Int Int
buildByteOffsets txt = listArray (0, n) offsets
  where
    n = T.length txt
    offsets = scanl (\byteOff c -> byteOff + utf8Len c) 0 (T.unpack txt)

buildLineStarts :: Text -> UArray Int Int
buildLineStarts txt = listArray (1, numLines) starts
  where
    chars = T.unpack txt
    byteOffs = scanl (\off c -> off + utf8Len c) 0 chars
    starts = 0 : [nextOff | (c, nextOff) <- zip chars (drop 1 byteOffs), c == '\n']
    numLines = length starts

runMuninnParser :: Parser a -> FilePath -> Text -> Either String a
runMuninnParser p name input =
    let result = evalState (runParserT (setTabWidth1 *> sc *> p <* eof) name input) (initialState input)
     in case result of
            Left err -> Left (errorBundlePretty err)
            Right a -> Right a
  where
    setTabWidth1 = MP.updateParserState $ \s ->
        s{MP.statePosState = (MP.statePosState s){MP.pstateTabWidth = MP.mkPos 1}}

getPos :: Parser SrcPos
getPos = do
    sp <- MP.getSourcePos
    charOff <- getOffset
    st <- lift get
    let byteOff = psByteOffsets st ! charOff
    -- Use our precomputed line-start array instead of Megaparsec's column,
    -- because Megaparsec counts UTF-16 code units for supplementary
    -- characters (e.g. emoji), causing wrong columns for BMP+ chars.
    let lineNum = unPos (sourceLine sp)
    let lineStartByteOff = psLineStarts st ! lineNum
    let byteCol = byteOff - lineStartByteOff + 1
    pure $ SrcPos byteOff lineNum byteCol

endPos :: Parser SrcPos
endPos = psEndPos <$> lift get

spanFrom :: SrcPos -> Parser SrcSpan
spanFrom start = SrcSpan start <$> endPos

(<->) :: SrcSpan -> SrcSpan -> SrcSpan
(SrcSpan s1 _) <-> (SrcSpan _ e2) = SrcSpan s1 e2

{- | Like 'try', but also saves/restores monadic PState on failure.
  Megaparsec's 'try' only backtracks the stream position, not State.
-}
tryState :: Parser a -> Parser a
tryState p = do
    savedSt <- lift get
    r <- optional (try p)
    case r of
        Just x -> pure x
        Nothing -> do
            lift $ put savedSt
            MP.empty

data RawComment = RawComment
    { rcStartPos :: !SrcPos
    , rcText :: !Text
    , rcStartLine :: !Int
    , rcEndLine :: !Int
    }

commentEndPos :: SrcPos -> Text -> SrcPos
commentEndPos start txt
    | T.length txt >= 2 && T.index txt 1 == '*' =
        T.foldl' step start txt
    | otherwise =
        let byteLen = T.foldl' (\acc c -> acc + utf8Len c) 0 txt
         in start{posOffset = posOffset start + byteLen, posCol = posCol start + byteLen}
  where
    step pos c
        | c == '\n' = pos{posOffset = posOffset pos + 1, posLine = posLine pos + 1, posCol = 1}
        | otherwise = let bl = utf8Len c in pos{posOffset = posOffset pos + bl, posCol = posCol pos + bl}

data SCItem = SCSpace !Bool | SCTag !Int | SCComment !RawComment

sc :: Parser ()
sc = do
    ln0 <- sourceLine <$> MP.getSourcePos
    st0 <- lift get
    (hasCont, lastSameLineOff, rawComments, lastTagLine, tagBeforeComments) <- scLoop False ln0 Nothing [] Nothing False
    ln1 <- sourceLine <$> MP.getSourcePos
    st <- lift get
    remaining <- getInput
    -- Only use the file tag line for nextTokenLine when the tag appears AFTER
    -- all comments; #+build before comments must not shift nextTokenLine.
    let realNextLine = unPos ln1
    let lastCommentLine = case rawComments of
            (rc : _) -> rcStartLine rc
            [] -> 0
    let nextTokenLine = case lastTagLine of
            Just tl | tl > lastCommentLine -> min realNextLine tl
            _ -> realNextLine
    let atEof = T.null remaining

    unless (null rawComments) $ do
        let existing = psCommentGroups st
        let seenOffsets = concatMap (\(CommentGroup _ cs) -> map (posOffset . commentPos) cs) existing
        let revsRaw = reverse rawComments
        let freshRaw = filter (\rc -> posOffset (rcStartPos rc) `notElem` seenOffsets) revsRaw
        unless (null freshRaw) $ do
            let adjusted = adjustEndLines freshRaw nextTokenLine
            let prevTokLine = psPrevTokLine st
            let autoSemi = psAutoSemi st
            let (groups, lineC, leadC) = groupComments prevTokLine nextTokenLine atEof autoSemi tagBeforeComments adjusted
            lift $ modify $ \s ->
                s
                    { psCommentGroups = groups ++ psCommentGroups s
                    , psLeadComment = leadC
                    , psLineComment = lineC
                    }

    when (not hasCont && (ln1 > ln0 || atEof) && psAutoSemi st && psExprLevel st == 0) $ do
        let basePos = case lastSameLineOff of
                Just off ->
                    let byteOff = psByteOffsets st0 ! off
                        ep = psEndPos st
                        lineStartByteOff = psLineStarts st0 ! posLine ep
                        col = byteOff - lineStartByteOff + 1
                     in SrcPos byteOff (posLine ep) col
                Nothing -> psEndPos st
        lift $ modify $ \s ->
            s
                { psPendingSemi = True
                , psSemiEndPos = basePos{posOffset = posOffset basePos + 1, posCol = posCol basePos + 1}
                }
  where
    isSameLineSpace c = c == ' ' || c == '\t' || c == '\r'

    scLoop cont ln0 lastOff rcs tagLine tagBefore = do
        r <-
            optional $
                choice
                    [ SCSpace False <$ takeWhile1P (Just "whitespace") isSameLineSpace
                    , SCSpace False <$ MP.single '\n'
                    , SCSpace True <$ backslashContinuation
                    , SCTag <$> captureFileTagLine
                    , SCComment <$> captureLineComment
                    , SCComment <$> captureBlockComment
                    ]
        case r of
            Just (SCSpace hasCont) -> do
                ln <- sourceLine <$> MP.getSourcePos
                off <- getOffset
                let newLastOff = if ln == ln0 then Just off else lastOff
                scLoop (cont || hasCont) ln0 newLastOff rcs tagLine tagBefore
            Just (SCTag tl) -> do
                ln <- sourceLine <$> MP.getSourcePos
                off <- getOffset
                let newLastOff = if ln == ln0 then Just off else lastOff
                let newTagLine = Just tl
                let newTagBefore = tagBefore || null rcs
                scLoop cont ln0 newLastOff rcs newTagLine newTagBefore
            Just (SCComment rc) -> do
                ln <- sourceLine <$> MP.getSourcePos
                off <- getOffset
                let newLastOff = if ln == ln0 then Just off else lastOff
                scLoop cont ln0 newLastOff (rc : rcs) tagLine tagBefore
            Nothing -> pure (cont, lastOff, rcs, tagLine, tagBefore)

    backslashContinuation = try $ do
        void $ MP.single '\\'
        void $ optional $ MP.single '\r'
        void $ MP.single '\n'

    captureLineComment :: Parser RawComment
    captureLineComment = do
        startPos <- getPos
        (raw, _) <- match (string "//" *> takeWhileP Nothing (\c -> c /= '\n'))
        let text = T.dropWhileEnd (== '\r') raw
        pure $ RawComment startPos text (posLine startPos) (posLine startPos)

    captureBlockComment :: Parser RawComment
    captureBlockComment = do
        startPos <- getPos
        (raw, _) <- match (L.skipBlockCommentNested "/*" "*/")
        let startLine = posLine startPos
        let nlCount = T.count "\n" raw
        pure $ RawComment startPos raw startLine (startLine + nlCount)

    captureFileTagLine :: Parser Int
    captureFileTagLine = do
        ln <- unPos . sourceLine <$> MP.getSourcePos
        L.skipLineComment "#+"
        pure ln

adjustEndLines :: [RawComment] -> Int -> [RawComment]
adjustEndLines [] _ = []
adjustEndLines [rc] nextTokLine =
    [rc{rcEndLine = rcEndLine rc + if nextTokLine > rcStartLine rc then 1 else 0}]
adjustEndLines (rc : rcs@(next : _)) nextTokLine =
    rc{rcEndLine = rcEndLine rc + if rcStartLine next > rcStartLine rc then 1 else 0}
        : adjustEndLines rcs nextTokLine

groupComments ::
    Int ->
    Int ->
    Bool ->
    Bool ->
    Bool ->
    [RawComment] ->
    ([CommentGroup], Maybe CommentGroup, Maybe CommentGroup)
groupComments _ _ _ _ _ [] = ([], Nothing, Nothing)
groupComments prevTokLine nextTokenLine atEof autoSemi tagBefore rcs@(first : _) =
    let
        (lineResult, rest1)
            | rcStartLine first == prevTokLine && prevTokLine /= 0 =
                if autoSemi
                    then
                        let (grp, endLn, remaining) = consumeSameLine prevTokLine rcs
                         in (Just (grp, endLn), remaining)
                    else
                        let (grp, endLn, remaining) = consumeGroup 0 rcs
                         in (Just (grp, endLn), remaining)
            | otherwise = (Nothing, rcs)

        isLineComment endLn = case rest1 of
            (next : _) ->
                rcStartLine next /= endLn
                    || nextTokenLine == prevTokLine + 1
                    || atEof
            [] ->
                nextTokenLine /= endLn
                    || nextTokenLine == prevTokLine + 1
                    || atEof

        lineC = case lineResult of
            Just (cg, endLn) | isLineComment endLn -> Just cg
            _ -> Nothing

        phase2Results = consumeAllGroups 1 rest1

        leadC
            | tagBefore = Nothing
            | otherwise = case reverse phase2Results of
                [] -> Nothing
                ((lastG, lastEndLn) : _) ->
                    if lastEndLn + 1 >= nextTokenLine
                        then Just lastG
                        else Nothing

        allGroups = maybe id ((:) . fst) lineResult $ map fst phase2Results
     in
        (allGroups, lineC, leadC)
  where
    consumeAllGroups :: Int -> [RawComment] -> [(CommentGroup, Int)]
    consumeAllGroups _ [] = []
    consumeAllGroups n rcs' =
        let (grp, endLn, rest) = consumeGroup n rcs'
         in (grp, endLn) : consumeAllGroups n rest

consumeGroup :: Int -> [RawComment] -> (CommentGroup, Int, [RawComment])
consumeGroup n (rc : rcs) = go rc [rc] (rcEndLine rc) rcs
  where
    go first acc endLn (next : rest)
        | rcStartLine next <= endLn + n =
            go first (acc ++ [next]) (rcEndLine next) rest
    go first acc endLn remaining =
        let comments = map (\r -> Comment (rcStartPos r) (rcText r)) acc
            startP = rcStartPos first
            lastRc = lastDef first acc
            endP = commentEndPos (rcStartPos lastRc) (rcText lastRc)
         in (CommentGroup (SrcSpan startP endP) comments, endLn, remaining)
consumeGroup _ [] = (CommentGroup (SrcSpan p0 p0) [], 0, [])
  where
    p0 = SrcPos 0 0 0

consumeSameLine :: Int -> [RawComment] -> (CommentGroup, Int, [RawComment])
consumeSameLine line (rc : rcs) = go rc [rc] (rcEndLine rc) rcs
  where
    go first acc _endLn (next : rest)
        | rcStartLine next == line =
            go first (acc ++ [next]) (rcEndLine next) rest
    go first acc endLn remaining =
        let comments = map (\r -> Comment (rcStartPos r) (rcText r)) acc
            startP = rcStartPos first
            lastRc = lastDef first acc
            endP = commentEndPos (rcStartPos lastRc) (rcText lastRc)
         in (CommentGroup (SrcSpan startP endP) comments, endLn, remaining)
consumeSameLine _ [] = (CommentGroup (SrcSpan p0 p0) [], 0, [])
  where
    p0 = SrcPos 0 0 0

lastDef :: a -> [a] -> a
lastDef def [] = def
lastDef _ xs = last xs

semi :: Parser ()
semi = do
    st <- lift get
    if psPendingSemi st
        then do
            lift $ modify $ \s ->
                s
                    { psPendingSemi = False
                    , psAutoSemi = False
                    , psEndPos = psSemiEndPos st
                    }
        else
            if psAutoSemi st
                then do
                    -- '}' and EOF trigger auto-semicolons even without a newline
                    isAtEnd <- MP.atEnd
                    if isAtEnd
                        then lift $ modify $ \s -> s{psAutoSemi = False}
                        else do
                            next <- MP.lookAhead (MP.anySingle)
                            if next == '}'
                                then lift $ modify $ \s -> s{psAutoSemi = False}
                                else void $ tok False (string ";")
                else void $ tok False (string ";")

clearPendingSemi :: Parser ()
clearPendingSemi =
    lift $ modify $ \s -> s{psPendingSemi = False}

tok :: Bool -> Parser a -> Parser a
tok isSemi p = do
    r <- p
    ep <- getPos
    lift $ modify $ \s -> s{psAutoSemi = isSemi, psEndPos = ep, psPrevTokLine = posLine ep}
    sc
    pure r

lparen :: Parser ()
lparen = do
    tok False (void $ char '(')
    lift $ modify $ \s -> s{psExprLevel = psExprLevel s + 1}

rparen :: Parser ()
rparen = do
    lift $ modify $ \s -> s{psExprLevel = max 0 (psExprLevel s - 1)}
    tok True (void $ char ')')

lbracket :: Parser ()
lbracket = do
    tok False (void $ char '[')
    lift $ modify $ \s -> s{psExprLevel = psExprLevel s + 1}

rbracket :: Parser ()
rbracket = do
    lift $ modify $ \s -> s{psExprLevel = max 0 (psExprLevel s - 1)}
    tok True (void $ char ']')

-- Braces don't affect exprLevel (auto-semis work inside blocks)
lbrace :: Parser ()
lbrace = tok False (void $ char '{')

rbrace :: Parser ()
rbrace = tok True (void $ char '}')

comma :: Parser ()
comma = tok False (void $ char ',')

colon :: Parser ()
colon = tok False (void $ char ':')

dot :: Parser ()
dot = tok False $ do
    void $ char '.'
    notFollowedBy (char '.')

op :: Text -> Parser ()
op s = tok (isAutoSemiOp s) $ try $ do
    void $ string s
    case T.unpack s of
        "=" -> notFollowedBy (char '=')
        "!" -> notFollowedBy (char '=')
        "+" -> notFollowedBy (char '=')
        "-" -> notFollowedBy (satisfy (\c -> c == '=' || c == '>' || c == '-'))
        "*" -> notFollowedBy (char '=')
        "/" -> notFollowedBy (satisfy (\c -> c == '=' || c == '/' || c == '*'))
        "%" -> notFollowedBy (satisfy (\c -> c == '=' || c == '%'))
        "&" -> notFollowedBy (satisfy (\c -> c == '=' || c == '&' || c == '~'))
        "|" -> notFollowedBy (satisfy (\c -> c == '=' || c == '|'))
        "~" -> notFollowedBy (char '=')
        "<" -> notFollowedBy (satisfy (\c -> c == '=' || c == '<'))
        ">" -> notFollowedBy (satisfy (\c -> c == '=' || c == '>'))
        "&&" -> notFollowedBy (char '=')
        "||" -> notFollowedBy (char '=')
        "%%" -> notFollowedBy (char '=')
        "<<" -> notFollowedBy (char '=')
        ">>" -> notFollowedBy (char '=')
        "&~" -> notFollowedBy (char '=')
        ".." -> notFollowedBy (satisfy (\c -> c == '<' || c == '='))
        "." -> notFollowedBy (char '.')
        _ -> pure ()
  where
    isAutoSemiOp "^" = True
    isAutoSemiOp "?" = True
    isAutoSemiOp "---" = True
    isAutoSemiOp _ = False

reservedWords :: Set.Set Text
reservedWords =
    Set.fromList
        [ "import"
        , "foreign"
        , "package"
        , "typeid"
        , "when"
        , "where"
        , "if"
        , "else"
        , "for"
        , "switch"
        , "in"
        , "not_in"
        , "do"
        , "case"
        , "break"
        , "continue"
        , "fallthrough"
        , "defer"
        , "return"
        , "proc"
        , "struct"
        , "union"
        , "enum"
        , "bit_set"
        , "bit_field"
        , "map"
        , "dynamic"
        , "auto_cast"
        , "cast"
        , "transmute"
        , "distinct"
        , "using"
        , "context"
        , "or_else"
        , "or_return"
        , "or_break"
        , "or_continue"
        , "asm"
        , "inline"
        , "no_inline"
        , "matrix"
        ]

autoSemiKeywords :: Set.Set Text
autoSemiKeywords =
    Set.fromList
        [ "break"
        , "continue"
        , "fallthrough"
        , "return"
        , "or_return"
        , "or_break"
        , "or_continue"
        , "context"
        , "typeid"
        ]

keyword :: Text -> Parser ()
keyword kw = tok (Set.member kw autoSemiKeywords) $ try $ do
    void $ string kw
    notFollowedBy (satisfy isIdentChar)

ident :: Parser Text
ident = tok True $ try $ do
    name <- rawIdent
    if Set.member name reservedWords
        then fail $ "keyword " ++ T.unpack name ++ " used as identifier"
        else pure name

rawIdent :: Parser Text
rawIdent = do
    c <- satisfy isIdentStart <?> "identifier"
    rest <- takeWhileP Nothing isIdentChar
    pure (T.cons c rest)

identOrKeyword :: Parser Text
identOrKeyword = tok True rawIdent

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlpha c || isDigit c || c == '_'

pInteger :: Parser (Text, Integer)
pInteger = tok True $ try $ do
    (raw, n) <- match numericLit
    notFollowedBy (try (char '.' *> notFollowedBy (char '.') *> pure '.'))
    notFollowedBy (satisfy (\c -> c == 'i' || c == 'I' || c == 'j' || c == 'J' || c == 'k' || c == 'K' || c == 'e' || c == 'E' || c == 'p' || c == 'P'))
    pure (raw, n)

pFloat :: Parser (Text, Double)
pFloat = tok True $ try $ do
    (raw, val) <- match floatLit
    notFollowedBy (satisfy (\c -> c == 'i' || c == 'I' || c == 'j' || c == 'J' || c == 'k' || c == 'K'))
    pure (raw, val)

pImag :: Parser (Text, Double)
pImag = tok True $ do
    (raw, val) <- match $ do
        v <- (fromIntegral <$> try numericLit) <|> floatLit
        void $ satisfy (\c -> c == 'i' || c == 'I' || c == 'j' || c == 'J' || c == 'k' || c == 'K')
        pure v
    pure (raw, val)

numericLit :: Parser Integer
numericLit =
    choice
        [ try hexInt
        , try hexFloatInt
        , try octInt
        , try binInt
        , decInt
        ]

hexFloatInt :: Parser Integer
hexFloatInt = do
    void $ char '0'
    void $ satisfy (\c -> c == 'h' || c == 'H')
    digits <- takeWhile1P (Just "hex digit") (\c -> isHexDigit c || c == '_')
    pure $ parseHex digits

decInt :: Parser Integer
decInt = do
    d <- satisfy isDigit
    rest <- takeWhileP Nothing (\c -> isDigit c || c == '_')
    pure $ parseDecimal (T.cons d rest)

hexInt :: Parser Integer
hexInt = do
    void $ char '0'
    void $ satisfy (\c -> c == 'x' || c == 'X')
    digits <- takeWhile1P (Just "hex digit") (\c -> isHexDigit c || c == '_')
    pure $ parseHex digits

octInt :: Parser Integer
octInt = do
    void $ char '0'
    void $ satisfy (\c -> c == 'o' || c == 'O')
    digits <- takeWhile1P (Just "octal digit") (\c -> (c >= '0' && c <= '7') || c == '_')
    pure $ parseOct digits

binInt :: Parser Integer
binInt = do
    void $ char '0'
    void $ satisfy (\c -> c == 'b' || c == 'B')
    digits <- takeWhile1P (Just "binary digit") (\c -> c == '0' || c == '1' || c == '_')
    pure $ parseBin digits

floatLit :: Parser Double
floatLit = try hexFloatLit <|> decFloatLit

decFloatLit :: Parser Double
decFloatLit = do
    raw <- try withDot <|> withoutDot
    pure $ parseDoubleOrZero raw
  where
    withDot = do
        intPart <- takeWhile1P Nothing (\c -> isDigit c || c == '_')
        void $ char '.'
        MP.notFollowedBy (char '.')
        fracPart <- takeWhileP Nothing (\c -> isDigit c || c == '_') -- empty fracPart valid: "1."
        expPart <- optional pExponent
        pure $ intPart <> "." <> fracPart <> fromMaybe "" expPart
    withoutDot = do
        intPart <- takeWhile1P Nothing (\c -> isDigit c || c == '_')
        expPart <- pExponent
        pure $ intPart <> expPart
    pExponent = do
        e <- T.singleton <$> satisfy (\c -> c == 'e' || c == 'E')
        sign <- option "" (T.singleton <$> satisfy (\c -> c == '+' || c == '-'))
        digits <- takeWhile1P Nothing (\c -> isDigit c || c == '_')
        pure $ e <> sign <> digits

hexFloatLit :: Parser Double
hexFloatLit = do
    raw <- do
        prefix <- string "0x" <|> string "0X"
        intPart <- takeWhile1P Nothing (\c -> isHexDigit c || c == '_')
        rest <- try withHexDot <|> withoutHexDot
        pure $ prefix <> intPart <> rest
    pure $ parseDoubleOrZero raw
  where
    withHexDot = do
        void $ char '.'
        fracPart <- takeWhile1P Nothing (\c -> isHexDigit c || c == '_')
        expPart <- optional pHexExponent
        pure $ "." <> fracPart <> fromMaybe "" expPart
    withoutHexDot = do
        expPart <- pHexExponent
        pure expPart
    pHexExponent = do
        p <- T.singleton <$> satisfy (\c -> c == 'p' || c == 'P')
        sign <- option "" (T.singleton <$> satisfy (\c -> c == '+' || c == '-'))
        digits <- takeWhile1P Nothing (\c -> isDigit c || c == '_')
        pure $ p <> sign <> digits

parseDecimal :: Text -> Integer
parseDecimal = T.foldl' step 0
  where
    step acc c
        | c == '_' = acc
        | otherwise = acc * 10 + toInteger (digitToInt c)

parseHex :: Text -> Integer
parseHex = T.foldl' step 0
  where
    step acc c
        | c == '_' = acc
        | otherwise = acc * 16 + toInteger (digitToInt c)

parseOct :: Text -> Integer
parseOct = T.foldl' step 0
  where
    step acc c
        | c == '_' = acc
        | otherwise = acc * 8 + toInteger (digitToInt c)

parseBin :: Text -> Integer
parseBin = T.foldl' step 0
  where
    step acc c
        | c == '_' = acc
        | c == '1' = acc * 2 + 1
        | otherwise = acc * 2

{- | Parse a Double from raw text, returning 0 for Odin-valid literals that
Haskell's Read cannot parse (e.g. "1.", "0x1p0"). The Double value is never
used by callers — only the raw Text is kept in the AST.
-}
parseDoubleOrZero :: Text -> Double
parseDoubleOrZero raw = fromMaybe 0 (readMaybe (filter (/= '_') (T.unpack raw)))

pString :: Parser Text
pString =
    tok True $
        choice
            [ char '"' *> (T.pack <$> manyTill pStringChar (char '"'))
            , char '`' *> (T.pack <$> manyTill anySingle (char '`'))
            ]

pStringRaw :: Parser Text
pStringRaw = tok True $ do
    (raw, _) <-
        match $
            choice
                [ char '"' *> manyTill pStringChar (char '"')
                , char '`' *> manyTill anySingle (char '`')
                ]
    pure raw

pStringChar :: Parser Char
pStringChar = (char '\\' *> pEscapeChar) <|> anySingle

pEscapeChar :: Parser Char
pEscapeChar =
    choice
        [ '\n' <$ char 'n'
        , '\t' <$ char 't'
        , '\r' <$ char 'r'
        , '\a' <$ char 'a'
        , '\b' <$ char 'b'
        , '\x1b' <$ char 'e'
        , '\f' <$ char 'f'
        , '\v' <$ char 'v'
        , '\\' <$ char '\\'
        , '"' <$ char '"'
        , '`' <$ char '`'
        , '\'' <$ char '\''
        , '\0' <$ char '0'
        , pHexEscape 'x' 2
        , pHexEscape 'u' 4
        , pHexEscape 'U' 8
        , anySingle
        ]

pHexEscape :: Char -> Int -> Parser Char
pHexEscape prefix n = do
    void $ char prefix
    digits <- count n (satisfy isHexDigit)
    pure $ chr $ foldl (\acc d -> acc * 16 + digitToInt d) 0 digits

pRuneRaw :: Parser Text
pRuneRaw = tok True $ do
    (raw, _) <- match $ do
        void $ char '\''
        void $ manyTill pRuneChar (char '\'')
        pure ()
    pure raw
  where
    pRuneChar = (char '\\' *> anySingle) <|> anySingle

pRune :: Parser Char
pRune = tok True $ do
    void $ char '\''
    c <- (char '\\' *> pEscapeChar) <|> anySingle
    void $ char '\''
    pure c
