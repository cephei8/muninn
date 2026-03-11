module Muninn.Parser.AST (
    File (..),
    Stmt (..),
    Expr (..),
    Field (..),
    FieldList (..),
    BitFieldField (..),
    Attribute (..),
    ForeignPath (..),
    Comment (..),
    CommentGroup (..),
    BinOp (..),
    UnaryOp (..),
    AssignOp (..),
    BranchKind (..),
    CastKind (..),
    LitKind (..),
    StructFlag (..),
    UnionFlag (..),
    FieldFlag (..),
    stmtSpan,
    exprSpan,
) where

import Data.Text (Text)
import Muninn.Parser.SrcLoc (SrcPos, SrcSpan (..))

data Comment = Comment
    { commentPos :: !SrcPos
    , commentText :: !Text
    }
    deriving (Show, Eq)

data CommentGroup = CommentGroup
    { cgSpan :: !SrcSpan
    , cgList :: [Comment]
    }
    deriving (Show, Eq)

data File a = File
    { fileAnn :: a
    , filePkg :: !Text
    , fileDecls :: [Stmt a]
    , fileComments :: [CommentGroup]
    , fileDocs :: Maybe CommentGroup
    }
    deriving (Show, Functor, Foldable, Traversable)

data Attribute a = Attribute
    { attrAnn :: a
    , attrExprs :: [Expr a]
    }
    deriving (Show, Functor, Foldable, Traversable)

data ForeignPath
    = ForeignStr !Text
    | ForeignIdent !Text
    | ForeignCond !ForeignPath !(Expr SrcSpan) !ForeignPath
    deriving (Show)

data Stmt a
    = ValueDecl a [Attribute a] [Expr a] (Maybe (Expr a)) [Expr a] !Bool
    | ImportDecl a [Attribute a] (Maybe Text) !Text
    | ForeignImportDecl a [Attribute a] !Text [ForeignPath]
    | ForeignBlockDecl a [Attribute a] (Maybe (Expr a)) (Stmt a)
    | PackageDecl a !Text
    | ExprStmt a (Expr a)
    | AssignStmt a [Expr a] !AssignOp [Expr a]
    | BlockStmt a (Maybe (Expr a)) [Stmt a] !Bool -- ^ True = do body
    | IfStmt a (Maybe (Expr a)) (Maybe (Stmt a)) (Expr a) (Stmt a) (Maybe (Stmt a))
    | WhenStmt a (Expr a) (Stmt a) (Maybe (Stmt a))
    | ForStmt a (Maybe (Expr a)) (Maybe (Stmt a)) (Maybe (Expr a)) (Maybe (Stmt a)) (Stmt a)
    | RangeStmt a (Maybe (Expr a)) [Expr a] (Expr a) (Stmt a) !Bool
    | SwitchStmt a (Maybe (Expr a)) (Maybe (Stmt a)) (Maybe (Expr a)) (Stmt a) !Bool
    | TypeSwitchStmt a (Maybe (Expr a)) (Maybe (Stmt a)) (Maybe (Expr a)) (Stmt a) !Bool
    | CaseClause a [Expr a] [Stmt a]
    | ReturnStmt a [Expr a]
    | DeferStmt a (Stmt a)
    | BranchStmt a !BranchKind (Maybe (Expr a))
    | UsingStmt a [Expr a]
    | DirectiveStmt a !Text (Stmt a)
    | BadStmt a
    deriving (Show, Functor, Foldable, Traversable)

data Expr a
    = Ident a !Text
    | BasicLit a !LitKind !Text
    | UnaryExpr a !UnaryOp (Maybe (Expr a))
    | BinaryExpr a (Expr a) !BinOp (Expr a)
    | ParenExpr a (Expr a)
    | CallExpr a (Expr a) [Expr a] !Bool
    | IndexExpr a (Expr a) (Expr a)
    | MatrixIndexExpr a (Expr a) (Expr a) (Expr a)
    | SliceExpr a (Expr a) (Maybe (Expr a)) (Maybe (Expr a))
    | SelectorExpr a (Expr a) (Expr a) !Bool -- ^ True = arrow call (->)
    | ImplicitSelectorExpr a (Expr a)
    | DerefExpr a (Expr a)
    | CompLit a (Maybe (Expr a)) [Expr a]
    | ProcLit a (Maybe Text) (Expr a) [Text] (Maybe (Stmt a))
    | TernaryIfExpr a !Bool (Expr a) (Expr a) (Expr a) -- ^ True = (? :) form, False = (val if cond else alt) postfix form
    | TernaryWhenExpr a (Expr a) (Expr a) (Expr a)
    | OrElseExpr a (Expr a) (Expr a)
    | OrReturnExpr a (Expr a)
    | OrBranchExpr a (Expr a) !BranchKind (Maybe (Expr a))
    | TypeAssertion a (Expr a) (Expr a)
    | TypeCast a !CastKind (Expr a) (Expr a)
    | AutoCast a (Expr a)
    | Ellipsis a (Maybe (Expr a))
    | FieldValue a (Expr a) (Expr a)
    | ProcGroup a [Expr a]
    | Undef a
    | Implicit a
    | BasicDirective a !Text (Maybe (Expr a))
    | TagExpr a !Text (Expr a)
    | PointerType a (Expr a) (Maybe (Expr a))
    | MultiPointerType a (Expr a)
    | ArrayType a (Maybe (Expr a)) (Expr a) (Maybe (Expr a))
    | DynamicArrayType a (Expr a) (Maybe (Expr a))
    | MapType a (Expr a) (Expr a)
    | StructType a (Maybe (FieldList a)) (FieldList a) (Maybe (Expr a)) [StructFlag]
    | UnionType a [Expr a] (Maybe (FieldList a)) [UnionFlag]
    | EnumType a (Maybe (Expr a)) [Expr a]
    | BitSetType a (Expr a) (Maybe (Expr a))
    | BitFieldType a (Maybe (Expr a)) [BitFieldField a]
    | ProcType a (FieldList a) (Maybe (FieldList a)) !Bool (Maybe Text) [Expr a] -- ^ Bool = diverging (-> !)
    | MatrixType a (Expr a) (Expr a) (Expr a)
    | DistinctType a (Expr a)
    | PolyType a (Expr a) (Maybe (Expr a))
    | TypeidType a (Maybe (Expr a))
    | HelperType a (Expr a)
    | RelativeType a (Expr a) (Expr a)
    | InlineAsmExpr a (Expr a) (Expr a)
    | BadExpr a
    deriving (Show, Functor, Foldable, Traversable)

data Field a = Field
    { fieldAnn :: a
    , fieldNames :: [Expr a]
    , fieldType :: Maybe (Expr a)
    , fieldDefault :: Maybe (Expr a)
    , fieldTag :: Maybe Text
    , fieldFlags :: [FieldFlag]
    }
    deriving (Show, Functor, Foldable, Traversable)

data FieldList a = FieldList
    { fieldListAnn :: a
    , fieldListFields :: [Field a]
    }
    deriving (Show, Functor, Foldable, Traversable)

data BitFieldField a = BitFieldField
    { bffAnn :: a
    , bffName :: Expr a
    , bffType :: Expr a
    , bffSize :: Expr a
    }
    deriving (Show, Functor, Foldable, Traversable)

data BinOp
    = OpAdd
    | OpSub
    | OpMul
    | OpQuo
    | OpMod
    | OpModMod
    | OpBitAnd
    | OpBitOr
    | OpBitXor
    | OpBitAndNot
    | OpShl
    | OpShr
    | OpLogAnd
    | OpLogOr
    | OpEq
    | OpNotEq
    | OpLt
    | OpGt
    | OpLtEq
    | OpGtEq
    | OpIn
    | OpNotIn
    | OpRangeHalf
    | OpRangeFull
    deriving (Show, Eq)

data UnaryOp
    = OpPos
    | OpNeg
    | OpNot
    | OpBitNot
    | OpAddr
    | OpDeref
    | OpDollar
    | OpQuestion
    deriving (Show, Eq)

data AssignOp
    = Assign
    | AddAssign
    | SubAssign
    | MulAssign
    | QuoAssign
    | ModAssign
    | ModModAssign
    | BitAndAssign
    | BitOrAssign
    | BitXorAssign
    | BitAndNotAssign
    | ShlAssign
    | ShrAssign
    | LogAndAssign
    | LogOrAssign
    | -- | type switch destructuring (switch v in x)
      InAssign
    deriving (Show, Eq)

data BranchKind = Break | Continue | Fallthrough
    deriving (Show, Eq)

data CastKind = CastNormal | Transmute
    deriving (Show, Eq)

data LitKind = LitInt | LitFloat | LitImag | LitRune | LitString
    deriving (Show, Eq)

data StructFlag = StructPacked | StructRawUnion
    deriving (Show, Eq)

data UnionFlag = UnionNoNil | UnionSharedNil
    deriving (Show, Eq)

data FieldFlag
    = FieldUsing
    | FieldAnyInt
    | FieldCVararg
    | FieldNoAlias
    | FieldSubtype
    | FieldConst
    | FieldByPtr
    | FieldNoBroadcast
    deriving (Show, Eq)

stmtSpan :: Stmt SrcSpan -> SrcSpan
stmtSpan = \case
    ValueDecl sp _ _ _ _ _ -> sp
    ImportDecl sp _ _ _ -> sp
    ForeignImportDecl sp _ _ _ -> sp
    ForeignBlockDecl sp _ _ _ -> sp
    PackageDecl sp _ -> sp
    ExprStmt sp _ -> sp
    AssignStmt sp _ _ _ -> sp
    BlockStmt sp _ _ _ -> sp
    IfStmt sp _ _ _ _ _ -> sp
    WhenStmt sp _ _ _ -> sp
    ForStmt sp _ _ _ _ _ -> sp
    RangeStmt sp _ _ _ _ _ -> sp
    SwitchStmt sp _ _ _ _ _ -> sp
    TypeSwitchStmt sp _ _ _ _ _ -> sp
    CaseClause sp _ _ -> sp
    ReturnStmt sp _ -> sp
    DeferStmt sp _ -> sp
    BranchStmt sp _ _ -> sp
    UsingStmt sp _ -> sp
    DirectiveStmt sp _ _ -> sp
    BadStmt sp -> sp

exprSpan :: Expr SrcSpan -> SrcSpan
exprSpan = \case
    Ident sp _ -> sp
    BasicLit sp _ _ -> sp
    UnaryExpr sp _ _ -> sp
    BinaryExpr sp _ _ _ -> sp
    ParenExpr sp _ -> sp
    CallExpr sp _ _ _ -> sp
    IndexExpr sp _ _ -> sp
    MatrixIndexExpr sp _ _ _ -> sp
    SliceExpr sp _ _ _ -> sp
    SelectorExpr sp _ _ _ -> sp
    ImplicitSelectorExpr sp _ -> sp
    DerefExpr sp _ -> sp
    CompLit sp _ _ -> sp
    ProcLit sp _ _ _ _ -> sp
    TernaryIfExpr sp _ _ _ _ -> sp
    TernaryWhenExpr sp _ _ _ -> sp
    OrElseExpr sp _ _ -> sp
    OrReturnExpr sp _ -> sp
    OrBranchExpr sp _ _ _ -> sp
    TypeAssertion sp _ _ -> sp
    TypeCast sp _ _ _ -> sp
    AutoCast sp _ -> sp
    Ellipsis sp _ -> sp
    FieldValue sp _ _ -> sp
    ProcGroup sp _ -> sp
    Undef sp -> sp
    Implicit sp -> sp
    BasicDirective sp _ _ -> sp
    TagExpr sp _ _ -> sp
    PointerType sp _ _ -> sp
    MultiPointerType sp _ -> sp
    ArrayType sp _ _ _ -> sp
    DynamicArrayType sp _ _ -> sp
    MapType sp _ _ -> sp
    StructType sp _ _ _ _ -> sp
    UnionType sp _ _ _ -> sp
    EnumType sp _ _ -> sp
    BitSetType sp _ _ -> sp
    BitFieldType sp _ _ -> sp
    ProcType sp _ _ _ _ _ -> sp
    MatrixType sp _ _ _ -> sp
    DistinctType sp _ -> sp
    PolyType sp _ _ -> sp
    TypeidType sp _ -> sp
    HelperType sp _ -> sp
    RelativeType sp _ _ -> sp
    InlineAsmExpr sp _ _ -> sp
    BadExpr sp -> sp
