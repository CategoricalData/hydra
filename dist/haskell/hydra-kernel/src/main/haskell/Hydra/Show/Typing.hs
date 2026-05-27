-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.typing types

module Hydra.Show.Typing where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Show a type constraint as a string
typeConstraint :: Typing.TypeConstraint -> String
typeConstraint tc =

      let ltyp = Typing.typeConstraintLeft tc
          rtyp = Typing.typeConstraintRight tc
      in (Strings.cat [
        ShowCore.type_ ltyp,
        "\8801",
        (ShowCore.type_ rtyp)])
-- | Show a type substitution as a string
typeSubst :: Typing.TypeSubst -> String
typeSubst ts =

      let subst = Typing.unTypeSubst ts
          pairs = Maps.toList subst
          showPair =
                  \pair ->
                    let name = Core.unName (Pairs.first pair)
                        typ = Pairs.second pair
                    in (Strings.cat [
                      name,
                      "\8614",
                      (ShowCore.type_ typ)])
          pairStrs = Lists.map showPair pairs
      in (Strings.cat [
        "{",
        (Strings.intercalate "," pairStrs),
        "}"])
