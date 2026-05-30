-- Note: this is an automatically generated file. Do not edit.
-- | Registry of Hydra's built-in type classes.

module Hydra.Classes where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | The equality type class: instances support structural equality.
equality :: Typing.TypeClass
equality = Typing.TypeClass {
  Typing.typeClassDescription = "Equality: instances support structural equality."}
-- | The ordering type class: instances support total ordering (and equality).
ordering :: Typing.TypeClass
ordering =
    Typing.TypeClass {
      Typing.typeClassDescription = "Ordering: instances support total ordering (and equality)."}
