{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Muninn.Parser.JSON
  ( fileToJSON
  ) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Text (Text)
import Muninn.Parser.AST
import Muninn.Parser.SrcLoc (SrcPos (..), SrcSpan (..))

fileToJSON :: File SrcSpan -> Value
fileToJSON = toJSON

instance ToJSON Comment where
  toJSON (Comment pos text) = object
    [ "pos" .= pos
    , "text" .= text
    ]

instance ToJSON CommentGroup where
  toJSON (CommentGroup sp list) = object
    [ "end" .= spanEnd sp
    , "list" .= map toJSON list
    , "pos" .= spanStart sp
    ]

instance ToJSON (File SrcSpan) where
  toJSON (File _sp pkg decls comments docs) = object
    [ "comments" .= map toJSON comments
    , "decls" .= map toJSON decls
    , "docs" .= fmap toJSON docs
    , "end" .= zeroPos
    , "node" .= ("File" :: Text)
    , "package" .= pkg
    , "pos" .= zeroPos
    ]
    where
      zeroPos = SrcPos 0 0 0

instance ToJSON (Stmt SrcSpan) where
  toJSON = \case
    ValueDecl sp attrs names ty vals mut -> object
      [ "node" .= ("ValueDecl" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "attributes" .= map toJSON attrs
      , "names" .= map toJSON names
      , "type" .= fmap toJSON ty
      , "values" .= map toJSON vals
      , "is_mutable" .= mut
      ]
    ImportDecl sp alias path -> object
      [ "node" .= ("ImportDecl" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "alias" .= alias
      , "path" .= path
      ]
    ForeignImportDecl sp name paths -> object
      [ "node" .= ("ForeignImportDecl" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "name" .= name
      , "paths" .= paths
      ]
    ForeignBlockDecl sp lib body -> object
      [ "node" .= ("ForeignBlockDecl" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "library" .= fmap toJSON lib
      , "body" .= toJSON body
      ]
    PackageDecl sp name -> object
      [ "node" .= ("PackageDecl" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "name" .= name
      ]
    ExprStmt sp e -> object
      [ "node" .= ("ExprStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      ]
    AssignStmt sp lhs op rhs -> object
      [ "node" .= ("AssignStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "lhs" .= map toJSON lhs
      , "op" .= showAssignOp op
      , "rhs" .= map toJSON rhs
      ]
    BlockStmt sp label stmts -> object
      [ "node" .= ("BlockStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "label" .= fmap toJSON label
      , "stmts" .= map toJSON stmts
      ]
    IfStmt sp ini cond body els -> object
      [ "node" .= ("IfStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "init" .= fmap toJSON ini
      , "cond" .= toJSON cond
      , "body" .= toJSON body
      , "else" .= fmap toJSON els
      ]
    WhenStmt sp cond body els -> object
      [ "node" .= ("WhenStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "cond" .= toJSON cond
      , "body" .= toJSON body
      , "else" .= fmap toJSON els
      ]
    ForStmt sp ini cond post body -> object
      [ "node" .= ("ForStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "init" .= fmap toJSON ini
      , "cond" .= fmap toJSON cond
      , "post" .= fmap toJSON post
      , "body" .= toJSON body
      ]
    RangeStmt sp vals range body rev -> object
      [ "node" .= ("RangeStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "vals" .= map toJSON vals
      , "range" .= toJSON range
      , "body" .= toJSON body
      , "reverse" .= rev
      ]
    SwitchStmt sp ini tag body partial -> object
      [ "node" .= ("SwitchStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "init" .= fmap toJSON ini
      , "tag" .= fmap toJSON tag
      , "body" .= toJSON body
      , "partial" .= partial
      ]
    TypeSwitchStmt sp ini tag body partial -> object
      [ "node" .= ("TypeSwitchStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "init" .= fmap toJSON ini
      , "tag" .= fmap toJSON tag
      , "body" .= toJSON body
      , "partial" .= partial
      ]
    CaseClause sp exprs stmts -> object
      [ "node" .= ("CaseClause" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "exprs" .= map toJSON exprs
      , "stmts" .= map toJSON stmts
      ]
    ReturnStmt sp vals -> object
      [ "node" .= ("ReturnStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "values" .= map toJSON vals
      ]
    DeferStmt sp s -> object
      [ "node" .= ("DeferStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "stmt" .= toJSON s
      ]
    BranchStmt sp kind label -> object
      [ "node" .= ("BranchStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "kind" .= showBranchKind kind
      , "label" .= fmap toJSON label
      ]
    UsingStmt sp exprs -> object
      [ "node" .= ("UsingStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "exprs" .= map toJSON exprs
      ]
    BadStmt sp -> object
      [ "node" .= ("BadStmt" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      ]

instance ToJSON (Expr SrcSpan) where
  toJSON = \case
    Ident sp name -> object
      [ "node" .= ("Ident" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "name" .= name
      ]
    BasicLit sp kind val -> object
      [ "node" .= ("BasicLit" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "kind" .= showLitKind kind
      , "value" .= val
      ]
    UnaryExpr sp op e -> object
      [ "node" .= ("UnaryExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "op" .= showUnaryOp op
      , "expr" .= fmap toJSON e
      ]
    BinaryExpr sp l op r -> object
      [ "node" .= ("BinaryExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "lhs" .= toJSON l
      , "op" .= showBinOp op
      , "rhs" .= toJSON r
      ]
    ParenExpr sp e -> object
      [ "node" .= ("ParenExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      ]
    CallExpr sp fn args ellipsis -> object
      [ "node" .= ("CallExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "func" .= toJSON fn
      , "args" .= map toJSON args
      , "ellipsis" .= ellipsis
      ]
    IndexExpr sp e idx -> object
      [ "node" .= ("IndexExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      , "index" .= toJSON idx
      ]
    MatrixIndexExpr sp e row col -> object
      [ "node" .= ("MatrixIndexExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      , "row" .= toJSON row
      , "col" .= toJSON col
      ]
    SliceExpr sp e lo hi -> object
      [ "node" .= ("SliceExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      , "low" .= fmap toJSON lo
      , "high" .= fmap toJSON hi
      ]
    SelectorExpr sp e sel -> object
      [ "node" .= ("SelectorExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      , "sel" .= toJSON sel
      ]
    ImplicitSelectorExpr sp e -> object
      [ "node" .= ("ImplicitSelectorExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "sel" .= toJSON e
      ]
    DerefExpr sp e -> object
      [ "node" .= ("DerefExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      ]
    CompLit sp ty elems -> object
      [ "node" .= ("CompLit" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "type" .= fmap toJSON ty
      , "elems" .= map toJSON elems
      ]
    ProcLit sp ty body -> object
      [ "node" .= ("ProcLit" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "type" .= toJSON ty
      , "body" .= maybe Null toJSON body
      ]
    TernaryIfExpr sp cond then_ else_ -> object
      [ "node" .= ("TernaryIfExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "cond" .= toJSON cond
      , "then" .= toJSON then_
      , "else" .= toJSON else_
      ]
    TernaryWhenExpr sp cond then_ else_ -> object
      [ "node" .= ("TernaryWhenExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "cond" .= toJSON cond
      , "then" .= toJSON then_
      , "else" .= toJSON else_
      ]
    OrElseExpr sp e1 e2 -> object
      [ "node" .= ("OrElseExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e1
      , "or_else" .= toJSON e2
      ]
    OrReturnExpr sp e -> object
      [ "node" .= ("OrReturnExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      ]
    OrBranchExpr sp e kind label -> object
      [ "node" .= ("OrBranchExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      , "kind" .= showBranchKind kind
      , "label" .= fmap toJSON label
      ]
    TypeAssertion sp e ty -> object
      [ "node" .= ("TypeAssertion" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      , "type" .= toJSON ty
      ]
    TypeCast sp kind ty e -> object
      [ "node" .= ("TypeCast" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "kind" .= showCastKind kind
      , "type" .= toJSON ty
      , "expr" .= toJSON e
      ]
    AutoCast sp e -> object
      [ "node" .= ("AutoCast" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= toJSON e
      ]
    Ellipsis sp e -> object
      [ "node" .= ("Ellipsis" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "expr" .= fmap toJSON e
      ]
    FieldValue sp k v -> object
      [ "node" .= ("FieldValue" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "field" .= toJSON k
      , "value" .= toJSON v
      ]
    ProcGroup sp exprs -> object
      [ "node" .= ("ProcGroup" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "exprs" .= map toJSON exprs
      ]
    Undef sp -> object
      [ "node" .= ("Undef" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      ]
    Implicit sp -> object
      [ "node" .= ("Implicit" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      ]
    BasicDirective sp name e -> object
      [ "node" .= ("BasicDirective" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "name" .= name
      , "expr" .= fmap toJSON e
      ]
    TagExpr sp tag e -> object
      [ "node" .= ("TagExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "tag" .= tag
      , "expr" .= toJSON e
      ]
    PointerType sp e tag -> object $
      [ "node" .= ("PointerType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "elem" .= toJSON e
      ] ++ maybe [] (\t -> ["tag" .= toJSON t]) tag
    MultiPointerType sp e -> object
      [ "node" .= ("MultiPointerType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "elem" .= toJSON e
      ]
    ArrayType sp len elem_ tag -> object $
      [ "node" .= ("ArrayType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "len" .= fmap toJSON len
      , "elem" .= toJSON elem_
      ] ++ maybe [] (\t -> ["tag" .= toJSON t]) tag
    DynamicArrayType sp e tag -> object $
      [ "node" .= ("DynamicArrayType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "elem" .= toJSON e
      ] ++ maybe [] (\t -> ["tag" .= toJSON t]) tag
    MapType sp k v -> object
      [ "node" .= ("MapType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "key" .= toJSON k
      , "value" .= toJSON v
      ]
    StructType sp fields align flags -> object
      [ "node" .= ("StructType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "fields" .= toJSON fields
      , "align" .= fmap toJSON align
      , "flags" .= map showStructFlag flags
      ]
    UnionType sp variants params flags -> object
      [ "node" .= ("UnionType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "variants" .= map toJSON variants
      , "params" .= fmap toJSON params
      , "flags" .= map showUnionFlag flags
      ]
    EnumType sp backing fields -> object
      [ "node" .= ("EnumType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "backing_type" .= fmap toJSON backing
      , "fields" .= map toJSON fields
      ]
    BitSetType sp e backing -> object
      [ "node" .= ("BitSetType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "elem" .= toJSON e
      , "underlying" .= fmap toJSON backing
      ]
    BitFieldType sp backing fields -> object
      [ "node" .= ("BitFieldType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "backing_type" .= fmap toJSON backing
      , "fields" .= map toJSON fields
      ]
    ProcType sp params results cc -> object
      [ "node" .= ("ProcType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "params" .= toJSON params
      , "results" .= fmap toJSON results
      , "calling_convention" .= cc
      ]
    MatrixType sp rows cols elem_ -> object
      [ "node" .= ("MatrixType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "rows" .= toJSON rows
      , "cols" .= toJSON cols
      , "elem" .= toJSON elem_
      ]
    DistinctType sp e -> object
      [ "node" .= ("DistinctType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "type" .= toJSON e
      ]
    PolyType sp name spec -> object
      [ "node" .= ("PolyType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "name" .= toJSON name
      , "specialization" .= fmap toJSON spec
      ]
    TypeidType sp spec -> object
      [ "node" .= ("TypeidType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "specialization" .= fmap toJSON spec
      ]
    HelperType sp e -> object
      [ "node" .= ("HelperType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "type" .= toJSON e
      ]
    RelativeType sp tag e -> object
      [ "node" .= ("RelativeType" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "tag" .= toJSON tag
      , "type" .= toJSON e
      ]
    InlineAsmExpr sp params ret -> object
      [ "node" .= ("InlineAsmExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      , "params" .= toJSON params
      , "return" .= toJSON ret
      ]
    BadExpr sp -> object
      [ "node" .= ("BadExpr" :: Text)
      , "pos" .= spanStart sp
      , "end" .= spanEnd sp
      ]

instance ToJSON (Attribute SrcSpan) where
  toJSON (Attribute sp exprs) = object
    [ "node" .= ("Attribute" :: Text)
    , "pos" .= spanStart sp
    , "end" .= spanEnd sp
    , "exprs" .= map toJSON exprs
    ]

instance ToJSON (Field SrcSpan) where
  toJSON (Field sp names ty def tag flags) = object
    [ "node" .= ("Field" :: Text)
    , "pos" .= spanStart sp
    , "end" .= spanEnd sp
    , "names" .= map toJSON names
    , "type" .= fmap toJSON ty
    , "default" .= fmap toJSON def
    , "tag" .= tag
    , "flags" .= map showFieldFlag (filter serializableFlag flags)
    ]

instance ToJSON (FieldList SrcSpan) where
  toJSON (FieldList sp fields) = object
    [ "node" .= ("FieldList" :: Text)
    , "pos" .= spanStart sp
    , "end" .= spanEnd sp
    , "fields" .= map toJSON fields
    ]

instance ToJSON (BitFieldField SrcSpan) where
  toJSON (BitFieldField sp name ty size) = object
    [ "node" .= ("BitFieldField" :: Text)
    , "pos" .= spanStart sp
    , "end" .= spanEnd sp
    , "name" .= toJSON name
    , "type" .= toJSON ty
    , "size" .= toJSON size
    ]

showBinOp :: BinOp -> Text
showBinOp = \case
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpQuo -> "/"
  OpMod -> "%"
  OpModMod -> "%%"
  OpBitAnd -> "&"
  OpBitOr -> "|"
  OpBitXor -> "~"
  OpBitAndNot -> "&~"
  OpShl -> "<<"
  OpShr -> ">>"
  OpLogAnd -> "&&"
  OpLogOr -> "||"
  OpEq -> "=="
  OpNotEq -> "!="
  OpLt -> "<"
  OpGt -> ">"
  OpLtEq -> "<="
  OpGtEq -> ">="
  OpIn -> "in"
  OpNotIn -> "not_in"
  OpRangeHalf -> "..<"
  OpRangeFull -> "..="

showUnaryOp :: UnaryOp -> Text
showUnaryOp = \case
  OpPos -> "+"
  OpNeg -> "-"
  OpNot -> "!"
  OpBitNot -> "~"
  OpAddr -> "&"
  OpDeref -> "^"
  OpDollar -> "$"
  OpQuestion -> "?"

showAssignOp :: AssignOp -> Text
showAssignOp = \case
  Assign -> "="
  AddAssign -> "+="
  SubAssign -> "-="
  MulAssign -> "*="
  QuoAssign -> "/="
  ModAssign -> "%="
  ModModAssign -> "%%="
  BitAndAssign -> "&="
  BitOrAssign -> "|="
  BitXorAssign -> "~="
  BitAndNotAssign -> "&~="
  ShlAssign -> "<<="
  ShrAssign -> ">>="
  LogAndAssign -> "&&="
  LogOrAssign -> "||="
  InAssign -> "?"

showBranchKind :: BranchKind -> Text
showBranchKind = \case
  Break -> "break"
  Continue -> "continue"
  Fallthrough -> "fallthrough"

showCastKind :: CastKind -> Text
showCastKind = \case
  CastNormal -> "cast"
  Transmute -> "transmute"

showLitKind :: LitKind -> Text
showLitKind = \case
  LitInt -> "integer"
  LitFloat -> "float"
  LitImag -> "imaginary"
  LitRune -> "rune"
  LitString -> "string"

showStructFlag :: StructFlag -> Text
showStructFlag = \case
  StructPacked -> "packed"
  StructRawUnion -> "raw_union"

showUnionFlag :: UnionFlag -> Text
showUnionFlag = \case
  UnionNoNil -> "no_nil"
  UnionSharedNil -> "shared_nil"

-- | Flags serialized in JSON output (matching Odin reference parser)
serializableFlag :: FieldFlag -> Bool
serializableFlag FieldConst = False
serializableFlag FieldByPtr = False
serializableFlag FieldNoBroadcast = False
serializableFlag _ = True

showFieldFlag :: FieldFlag -> Text
showFieldFlag = \case
  FieldUsing -> "using"
  FieldAnyInt -> "any_int"
  FieldCVararg -> "c_vararg"
  FieldNoAlias -> "no_alias"
  FieldSubtype -> "subtype"
  FieldConst -> "const"
  FieldByPtr -> "by_ptr"
  FieldNoBroadcast -> "no_broadcast"
