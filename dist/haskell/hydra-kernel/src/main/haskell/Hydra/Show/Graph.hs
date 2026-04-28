-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.graph types

module Hydra.Show.Graph where
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as ShowCore
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Show a list of bindings as a string
graph :: [Core.Binding] -> String
graph elements =

      let elementStrs = Lists.map ShowCore.binding elements
      in (Strings.cat [
        "{",
        (Strings.intercalate ", " elementStrs),
        "}"])
