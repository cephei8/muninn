module Muninn.Formatter
  ( formatOdin
  , formatFile
  ) where

import Data.Text (Text)
import Muninn.Parser (parseFile)
import Muninn.Parser.AST
import Muninn.Parser.SrcLoc (SrcSpan)
import Muninn.Formatter.Print
import Muninn.Formatter.Stmt (fmtStmt)

formatOdin :: File SrcSpan -> Text
formatOdin file = runPrinter (fmtFile file)

formatFile :: FilePath -> IO (Either String Text)
formatFile path = do
  result <- parseFile path
  case result of
    Left err -> pure (Left err)
    Right ast -> pure (Right (formatOdin ast))

fmtFile :: File SrcSpan -> Printer ()
fmtFile (File _sp pkg decls _comments _docs) = do
  emit "package "
  emit pkg
  case decls of
    [] -> newline
    _ -> do
      newline
      newline
      mapM_ (\d -> fmtStmt d >> newline) decls
