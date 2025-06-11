module Hydra.Ext.Csharp.Utils where

import qualified Hydra.Ext.Csharp.Syntax as Cs
import Hydra.Formatting

normalizeComment :: String -> String
normalizeComment = stripLeadingAndTrailingWhitespace
