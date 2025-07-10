module Hydra.Ext.Staging.Csharp.Utils where

import Hydra.Kernel
import qualified Hydra.Ext.Csharp.Syntax as Cs
import Hydra.Formatting

normalizeComment :: String -> String
normalizeComment = stripLeadingAndTrailingWhitespace
