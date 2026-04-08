
module Hydra.Sources.Kernel.Terms.Extract.Util where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors as ShowError


ns :: Namespace
ns = Namespace "hydra.extract.util"

module_ :: Module
module_ = Module ns definitions
    [ExtractCore.ns, ShowError.ns]
    kernelTypesNamespaces $
    Just ("Extraction and validation for hydra.util types")
  where
   definitions = [
     toDefinition comparison]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

formatError :: TTerm (Error -> String)
formatError = "e" ~> ShowError.error_ @@ var "e"

comparison :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either Error Comparison)
comparison = define "comparison" $
  doc "Extract a comparison from a term" $
  "cx" ~> "graph" ~> "term" ~>
    "fname" <<~ ExtractCore.unitVariant @@ Core.nameLift _Comparison @@ var "graph" @@ var "term" $
        Logic.ifElse (Equality.equal (Core.unName $ var "fname") (string $ unName _Comparison_equalTo))
          (right Graph.comparisonEqualTo)
          (Logic.ifElse (Equality.equal (Core.unName $ var "fname") (string $ unName _Comparison_lessThan))
            (right Graph.comparisonLessThan)
            (Logic.ifElse (Equality.equal (Core.unName $ var "fname") (string $ unName _Comparison_greaterThan))
              (right Graph.comparisonGreaterThan)
              (Ctx.failInContext (Error.errorExtraction $ Error.extractionErrorUnexpectedShape $ Error.unexpectedShapeError (string "comparison") (Core.unName (var "fname"))) (var "cx"))))
