-- | All of Hydra's term-level kernel modules
module Hydra.Sources.Kernel.Terms.All(
  module Hydra.Sources.Kernel.Types.All,
  module Hydra.Sources.Kernel.Terms.All,
) where

import Hydra.Kernel
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Decoding        as Decoding
import qualified Hydra.Sources.Kernel.Terms.Describe.Core   as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Mantle as DescribeMantle
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Mantle  as ExtractMantle
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Mantle     as ShowMantle
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import qualified Hydra.Sources.Kernel.Terms.Variants        as Variants

kernelModules :: [Module]
kernelModules = kernelTypesModules ++ kernelTermsModules

kernelTermsModules :: [Module]
kernelTermsModules = [
  AdaptLiterals.module_,
  AdaptModules.module_,
  AdaptTerms.module_,
  AdaptUtils.module_,
  Annotations.module_,
  Arity.module_,
  Constants.module_,
  DecodeCore.module_,
  Decoding.module_,
  DescribeCore.module_,
  DescribeMantle.module_,
  EncodeCore.module_,
  ExtractCore.module_,
  ExtractMantle.module_,
  Formatting.module_,
  Grammars.module_,
  Inference.module_,
  Languages.module_,
  Lexical.module_,
  Literals.module_,
  Monads.module_,
  Names.module_,
  Reduction.module_,
  Rewriting.module_,
  Schemas.module_,
  Serialization.module_,
  ShowAccessors.module_,
  ShowCore.module_,
  ShowGraph.module_,
  ShowMantle.module_,
  ShowTyping.module_,
  Sorting.module_,
  Substitution.module_,
  Tarjan.module_,
  Templates.module_,
  Unification.module_,
  Variants.module_]
