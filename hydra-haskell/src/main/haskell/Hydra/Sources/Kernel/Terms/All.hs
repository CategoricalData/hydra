-- | All of Hydra's term-level kernel modules
module Hydra.Sources.Kernel.Terms.All where

import Hydra.Kernel

import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple    as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decoding        as Decoding
import qualified Hydra.Sources.Kernel.Terms.Encoding        as Encoding
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Helpers as ExtractHelpers
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Hoisting        as Hoisting
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Parsers         as Parsers
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect         as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors  as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta       as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Show.Util       as ShowUtil
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification

-- Secondary, generated decoding modules
import qualified Hydra.Sources.Decode.Accessors     as DecodeAccessors
import qualified Hydra.Sources.Decode.Ast           as DecodeAst
import qualified Hydra.Sources.Decode.Classes       as DecodeClasses
import qualified Hydra.Sources.Decode.Coders        as DecodeCoders
import qualified Hydra.Sources.Decode.Compute       as DecodeCompute
import qualified Hydra.Sources.Decode.Constraints   as DecodeConstraints
import qualified Hydra.Sources.Decode.Core          as DecodeCore
import qualified Hydra.Sources.Decode.Grammar       as DecodeGrammar
import qualified Hydra.Sources.Decode.Json.Model    as DecodeJson
import qualified Hydra.Sources.Decode.Module        as DecodeModule
import qualified Hydra.Sources.Decode.Parsing       as DecodeParsing
import qualified Hydra.Sources.Decode.Phantoms      as DecodePhantoms
import qualified Hydra.Sources.Decode.Query         as DecodeQuery
import qualified Hydra.Sources.Decode.Relational    as DecodeRelational
import qualified Hydra.Sources.Decode.Tabular       as DecodeTabular
import qualified Hydra.Sources.Decode.Testing       as DecodeTesting
import qualified Hydra.Sources.Decode.Topology      as DecodeTopology
import qualified Hydra.Sources.Decode.Typing        as DecodeTyping
import qualified Hydra.Sources.Decode.Util          as DecodeUtil
import qualified Hydra.Sources.Decode.Variants      as DecodeVariants
import qualified Hydra.Sources.Decode.Workflow      as DecodeWorkflow

-- Secondary, generated encoding modules
import qualified Hydra.Sources.Encode.Accessors     as EncodeAccessors
import qualified Hydra.Sources.Encode.Ast           as EncodeAst
import qualified Hydra.Sources.Encode.Classes       as EncodeClasses
import qualified Hydra.Sources.Encode.Coders        as EncodeCoders
import qualified Hydra.Sources.Encode.Compute       as EncodeCompute
import qualified Hydra.Sources.Encode.Constraints   as EncodeConstraints
import qualified Hydra.Sources.Encode.Core          as EncodeCore
import qualified Hydra.Sources.Encode.Grammar       as EncodeGrammar
import qualified Hydra.Sources.Encode.Json.Model    as EncodeJson
import qualified Hydra.Sources.Encode.Module        as EncodeModule
import qualified Hydra.Sources.Encode.Parsing       as EncodeParsing
import qualified Hydra.Sources.Encode.Phantoms      as EncodePhantoms
import qualified Hydra.Sources.Encode.Query         as EncodeQuery
import qualified Hydra.Sources.Encode.Relational    as EncodeRelational
import qualified Hydra.Sources.Encode.Tabular       as EncodeTabular
import qualified Hydra.Sources.Encode.Testing       as EncodeTesting
import qualified Hydra.Sources.Encode.Topology      as EncodeTopology
import qualified Hydra.Sources.Encode.Typing        as EncodeTyping
import qualified Hydra.Sources.Encode.Util          as EncodeUtil
import qualified Hydra.Sources.Encode.Variants      as EncodeVariants
import qualified Hydra.Sources.Encode.Workflow      as EncodeWorkflow


kernelTermsModules :: [Module]
kernelTermsModules = kernelPrimaryTermsModules ++ kernelDecodingModules ++ kernelEncodingModules

kernelPrimaryTermsModules :: [Module]
kernelPrimaryTermsModules = [
  AdaptLiterals.module_,
  AdaptModules.module_,
  AdaptSimple.module_,
  AdaptTerms.module_,
  AdaptUtils.module_,
  Annotations.module_,
  Arity.module_,
  Checking.module_,
  Constants.module_,
  Decoding.module_,
  Encoding.module_,
  ExtractCore.module_,
  ExtractHelpers.module_,
  ExtractUtil.module_,
  Formatting.module_,
  Grammars.module_,
  Hoisting.module_,
  Inference.module_,
  Languages.module_,
  Lexical.module_,
  Literals.module_,
  Monads.module_,
  Names.module_,
  Parsers.module_,
  Reduction.module_,
  Reflect.module_,
  Rewriting.module_,
  Schemas.module_,
  Serialization.module_,
  ShowAccessors.module_,
  ShowCore.module_,
  ShowGraph.module_,
  ShowMeta.module_,
  ShowTyping.module_,
  ShowUtil.module_,
  Sorting.module_,
  Substitution.module_,
  Tarjan.module_,
  Templates.module_,
  Unification.module_]

kernelDecodingModules :: [Module]
kernelDecodingModules = [
  DecodeAccessors.module_,
  DecodeAst.module_,
  DecodeClasses.module_,
  DecodeCoders.module_,
  DecodeCompute.module_,
  DecodeConstraints.module_,
  DecodeCore.module_,
  DecodeGrammar.module_,
  DecodeJson.module_,
  DecodeModule.module_,
  DecodeParsing.module_,
  DecodePhantoms.module_,
  DecodeQuery.module_,
  DecodeRelational.module_,
  DecodeTabular.module_,
  DecodeTesting.module_,
  DecodeTopology.module_,
  DecodeTyping.module_,
  DecodeUtil.module_,
  DecodeVariants.module_,
  DecodeWorkflow.module_]

kernelEncodingModules :: [Module]
kernelEncodingModules = [
  EncodeAccessors.module_,
  EncodeAst.module_,
  EncodeClasses.module_,
  EncodeCoders.module_,
  EncodeCompute.module_,
  EncodeConstraints.module_,
  EncodeCore.module_,
  EncodeGrammar.module_,
  EncodeJson.module_,
  EncodeModule.module_,
  EncodeParsing.module_,
  EncodePhantoms.module_,
  EncodeQuery.module_,
  EncodeRelational.module_,
  EncodeTabular.module_,
  EncodeTesting.module_,
  EncodeTopology.module_,
  EncodeTyping.module_,
  EncodeUtil.module_,
  EncodeVariants.module_,
  EncodeWorkflow.module_]
