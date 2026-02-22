module Muninn.Parser.AST
  ( File (..)
  , Stmt (..)
  , Expr (..)
  , Field (..)
  , FieldList (..)
  , BitFieldField (..)
  , Attribute (..)
  , Comment (..)
  , CommentGroup (..)
  , BinOp (..)
  , UnaryOp (..)
  , AssignOp (..)
  , BranchKind (..)
  , CastKind (..)
  , LitKind (..)
  , StructFlag (..)
  , UnionFlag (..)
  , FieldFlag (..)
  ) where

import Data.Text (Text)
import Muninn.Parser.SrcLoc (SrcPos, SrcSpan)

data Comment = Comment
  { commentPos  :: !SrcPos
  , commentText :: !Text
  }
  deriving (Show, Eq)

data CommentGroup = CommentGroup
  { cgSpan :: !SrcSpan
  , cgList :: [Comment]
  }
  deriving (Show, Eq)

-- | Top-level file
data File a = File
  { fileAnn      :: a
  , filePkg      :: !Text
  , fileDecls    :: [Stmt a]
  , fileComments :: [CommentGroup]
  , fileDocs     :: Maybe CommentGroup
  }
  deriving (Show, Functor, Foldable, Traversable)

-- | Attributes
data Attribute a = Attribute
  { attrAnn :: a
  , attrExprs :: [Expr a]
  }
  deriving (Show, Functor, Foldable, Traversable)

-- | Statements and declarations
data Stmt a
  = ValueDecl a [Attribute a] [Expr a] (Maybe (Expr a)) [Expr a] !Bool
    -- ^ ann, attributes, names, type, values, is_mutable
  | ImportDecl a (Maybe Text) !Text
    -- ^ ann, alias, path
  | ForeignImportDecl a !Text [Text]
    -- ^ ann, name, paths
  | ForeignBlockDecl a (Maybe (Expr a)) (Stmt a)
    -- ^ ann, library (Nothing for anonymous foreign blocks), body
  | PackageDecl a !Text
  | ExprStmt a (Expr a)
  | AssignStmt a [Expr a] !AssignOp [Expr a]
  | BlockStmt a (Maybe (Expr a)) [Stmt a]
    -- ^ ann, label, stmts
  | IfStmt a (Maybe (Stmt a)) (Expr a) (Stmt a) (Maybe (Stmt a))
    -- ^ ann, init, cond, body, else
  | WhenStmt a (Expr a) (Stmt a) (Maybe (Stmt a))
    -- ^ ann, cond, body, else
  | ForStmt a (Maybe (Stmt a)) (Maybe (Expr a)) (Maybe (Stmt a)) (Stmt a)
    -- ^ ann, init, cond, post, body
  | RangeStmt a [Expr a] (Expr a) (Stmt a) !Bool
    -- ^ ann, vals, range, body, reverse
  | SwitchStmt a (Maybe (Stmt a)) (Maybe (Expr a)) (Stmt a) !Bool
    -- ^ ann, init, tag, body, partial
  | TypeSwitchStmt a (Maybe (Stmt a)) (Maybe (Expr a)) (Stmt a) !Bool
    -- ^ ann, init, tag, body, partial
  | CaseClause a [Expr a] [Stmt a]
  | ReturnStmt a [Expr a]
  | DeferStmt a (Stmt a)
  | BranchStmt a !BranchKind (Maybe (Expr a))
    -- ^ ann, kind, label
  | UsingStmt a [Expr a]
  | BadStmt a
  deriving (Show, Functor, Foldable, Traversable)

-- | Expressions
data Expr a
  = Ident a !Text
  | BasicLit a !LitKind !Text
  | UnaryExpr a !UnaryOp (Maybe (Expr a))
  | BinaryExpr a (Expr a) !BinOp (Expr a)
  | ParenExpr a (Expr a)
  | CallExpr a (Expr a) [Expr a] !Bool
    -- ^ ann, func, args, has_ellipsis
  | IndexExpr a (Expr a) (Expr a)
  | MatrixIndexExpr a (Expr a) (Expr a) (Expr a)
    -- ^ ann, expr, row, col
  | SliceExpr a (Expr a) (Maybe (Expr a)) (Maybe (Expr a))
  | SelectorExpr a (Expr a) (Expr a)
  | ImplicitSelectorExpr a (Expr a)
  | DerefExpr a (Expr a)
  | CompLit a (Maybe (Expr a)) [Expr a]
    -- ^ ann, type, elems
  | ProcLit a (Expr a) (Maybe (Stmt a))
    -- ^ ann, type, body (Nothing for --- declarations)
  | TernaryIfExpr a (Expr a) (Expr a) (Expr a)
    -- ^ ann, cond, then, else
  | TernaryWhenExpr a (Expr a) (Expr a) (Expr a)
  | OrElseExpr a (Expr a) (Expr a)
  | OrReturnExpr a (Expr a)
  | OrBranchExpr a (Expr a) !BranchKind (Maybe (Expr a))
  | TypeAssertion a (Expr a) (Expr a)
  | TypeCast a !CastKind (Expr a) (Expr a)
    -- ^ ann, kind, type, expr
  | AutoCast a (Expr a)
  | Ellipsis a (Maybe (Expr a))
  | FieldValue a (Expr a) (Expr a)
  | ProcGroup a [Expr a]
  | Undef a
  | Implicit a
  | BasicDirective a !Text (Maybe (Expr a))
  | TagExpr a !Text (Expr a)
  -- Type expressions
  | PointerType a (Expr a) (Maybe (Expr a))
    -- ^ ann, pointee, tag (#soa)
  | MultiPointerType a (Expr a)
  | ArrayType a (Maybe (Expr a)) (Expr a) (Maybe (Expr a))
    -- ^ ann, length (Nothing=slice), elem, tag (#simd/#soa/#sparse)
  | DynamicArrayType a (Expr a) (Maybe (Expr a))
    -- ^ ann, elem, tag (#soa)
  | MapType a (Expr a) (Expr a)
  | StructType a (FieldList a) (Maybe (Expr a)) [StructFlag]
  | UnionType a [Expr a] (Maybe (FieldList a)) [UnionFlag]
  | EnumType a (Maybe (Expr a)) [Expr a]
  | BitSetType a (Expr a) (Maybe (Expr a))
  | BitFieldType a (Maybe (Expr a)) [BitFieldField a]
  | ProcType a (FieldList a) (Maybe (FieldList a)) (Maybe Text)
  | MatrixType a (Expr a) (Expr a) (Expr a)
    -- ^ ann, rows, cols, elem
  | DistinctType a (Expr a)
  | PolyType a (Expr a) (Maybe (Expr a))
  | TypeidType a (Maybe (Expr a))
  | HelperType a (Expr a)
  | RelativeType a (Expr a) (Expr a)
  | InlineAsmExpr a (Expr a) (Expr a)
  | BadExpr a
  deriving (Show, Functor, Foldable, Traversable)

-- | Fields
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

-- | Binary operators
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
  | OpRangeHalf   -- ..<
  | OpRangeFull   -- ..=
  deriving (Show, Eq)

-- | Unary operators
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

-- | Assignment operators
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
  | InAssign  -- ^ type switch destructuring (switch v in x)
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
