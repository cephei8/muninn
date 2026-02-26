module Muninn.Formatter.Support
  ( fmtField
  , fmtFieldList
  , fmtFieldListInline
  , fmtResultType
  , fmtBitFieldField
  , fmtAttribute
  , fmtFieldFlag
  , fmtComment
  , fmtCommentGroup
  ) where

import Muninn.Parser.AST
import Muninn.Parser.SrcLoc (SrcSpan)
import Muninn.Formatter.Print
import Muninn.Formatter.Stmt (fmtExpr)

fmtFieldFlag :: FieldFlag -> Printer ()
fmtFieldFlag = \case
  FieldUsing    -> emit "using "
  FieldAnyInt   -> emit "#any_int "
  FieldCVararg  -> emit "#c_vararg"
  FieldNoAlias  -> emit "#no_alias "
  FieldSubtype  -> emit "#subtype "
  FieldConst    -> emit "#const "
  FieldByPtr    -> emit "#by_ptr "
  FieldNoBroadcast -> emit "#no_broadcast "

fmtField :: Field SrcSpan -> Printer ()
fmtField (Field _sp names mTy mDef mTag flags) = do
  mapM_ fmtFieldFlag flags
  commaSep (map fmtExpr names)
  case (null names, mTy, mDef) of
    -- Unnamed param: just the type
    (True, Just ty, _) -> fmtExpr ty
    -- Named with type and default
    (False, Just ty, Just def) -> do
      emit ": "
      fmtExpr ty
      emit " = "
      fmtExpr def
    -- Named with type only
    (False, Just ty, Nothing) -> do
      emit ": "
      fmtExpr ty
    -- Named with default only (no type)
    (False, Nothing, Just def) -> do
      emit ": = "
      fmtExpr def
    -- Otherwise
    _ -> pure ()
  case mTag of
    Just tag -> do
      space
      emit tag
    Nothing -> pure ()

fmtFieldList :: FieldList SrcSpan -> Printer ()
fmtFieldList (FieldList _sp fields) = do
  mapM_ (\f -> newline >> fmtField f >> emit ",") fields

fmtFieldListInline :: FieldList SrcSpan -> Printer ()
fmtFieldListInline (FieldList _sp fields) =
  commaSep (map fmtField fields)

fmtResultType :: FieldList SrcSpan -> Printer ()
fmtResultType (FieldList _sp [Field _fsp [] (Just ty) Nothing Nothing []]) =
  fmtExpr ty
fmtResultType fl =
  parens (fmtFieldListInline fl)

fmtBitFieldField :: BitFieldField SrcSpan -> Printer ()
fmtBitFieldField (BitFieldField _sp name ty size) = do
  fmtExpr name
  emit ": "
  fmtExpr ty
  emit " | "
  fmtExpr size

fmtAttribute :: Attribute SrcSpan -> Printer ()
fmtAttribute (Attribute _sp exprs) = do
  emit "@"
  case exprs of
    [e] -> parens (fmtExpr e)
    _   -> parens (commaSep (map fmtExpr exprs))

fmtComment :: Comment -> Printer ()
fmtComment (Comment _pos text) = emit text

fmtCommentGroup :: CommentGroup -> Printer ()
fmtCommentGroup (CommentGroup _sp comments) =
  mapM_ (\c -> fmtComment c >> newline) comments
