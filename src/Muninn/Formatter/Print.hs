module Muninn.Formatter.Print
  ( Printer
  , runPrinter
  , emit
  , newline
  , space
  , withIndent
  , emitIndent
  , bracesBlock
  , commaSep
  , parens
  , brackets
  , sepBy
  ) where

import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.State.Strict (State, execState, get, put)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as B

type Printer a = ReaderT Int (State Builder) a

runPrinter :: Printer () -> Text
runPrinter p = TL.toStrict $ B.toLazyText $ execState (runReaderT p 0) mempty

emit :: Text -> Printer ()
emit t = do
  b <- get
  put (b <> B.fromText t)

emitIndent :: Printer ()
emitIndent = do
  depth <- ask
  emit (T.replicate depth "\t")

newline :: Printer ()
newline = do
  emit "\n"
  emitIndent

space :: Printer ()
space = emit " "

withIndent :: Printer a -> Printer a
withIndent = local (+ 1)

bracesBlock :: Printer () -> Printer ()
bracesBlock body = do
  emit " {"
  withIndent body
  newline
  emit "}"

parens :: Printer () -> Printer ()
parens body = do
  emit "("
  body
  emit ")"

brackets :: Printer () -> Printer ()
brackets body = do
  emit "["
  body
  emit "]"

commaSep :: [Printer ()] -> Printer ()
commaSep = sepBy (emit ", ")

sepBy :: Printer () -> [Printer ()] -> Printer ()
sepBy _ [] = pure ()
sepBy _ [x] = x
sepBy sep (x:xs) = do
  x
  mapM_ (\a -> sep >> a) xs
