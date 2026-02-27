module Muninn.Parser.SrcLoc (
    SrcPos (..),
    SrcSpan (..),
) where

import Data.Aeson (ToJSON (..), object, (.=))

data SrcPos = SrcPos
    { posOffset :: !Int
    , posLine :: !Int
    , posCol :: !Int
    }
    deriving (Show, Eq, Ord)

data SrcSpan = SrcSpan
    { spanStart :: !SrcPos
    , spanEnd :: !SrcPos
    }
    deriving (Show, Eq, Ord)

instance ToJSON SrcPos where
    toJSON (SrcPos off ln col) =
        object ["offset" .= off, "line" .= ln, "col" .= col]

instance ToJSON SrcSpan where
    toJSON (SrcSpan s e) =
        object ["start" .= s, "end" .= e]
