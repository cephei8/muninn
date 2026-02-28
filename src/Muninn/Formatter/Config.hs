module Muninn.Formatter.Config (
    FmtConfig (..),
    IndentStyle (..),
    defaultFmtConfig,
) where

data IndentStyle = Tabs | Spaces
    deriving (Show, Eq)

data FmtConfig = FmtConfig
    { cfgLineWidth :: !Int
    , cfgIndentWidth :: !Int
    , cfgNewlineLimit :: !Int
    , cfgIndentStyle :: !IndentStyle
    }
    deriving (Show, Eq)

defaultFmtConfig :: FmtConfig
defaultFmtConfig =
    FmtConfig
        { cfgLineWidth = 100
        , cfgIndentWidth = 4
        , cfgNewlineLimit = 2
        , cfgIndentStyle = Tabs
        }
