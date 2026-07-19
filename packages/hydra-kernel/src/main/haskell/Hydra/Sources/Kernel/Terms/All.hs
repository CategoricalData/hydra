-- | All of Hydra's term-level kernel modules
module Hydra.Sources.Kernel.Terms.All where

import Hydra.Kernel

import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.Analysis        as Analysis
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Classes         as Classes
import qualified Hydra.Sources.Kernel.Terms.Generation      as Generation
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decoding        as Decoding
import qualified Hydra.Sources.Kernel.Terms.Dependencies    as Dependencies
import qualified Hydra.Sources.Kernel.Terms.Differentiation as Differentiation
import qualified Hydra.Sources.Kernel.Terms.Dsls            as Dsls
import qualified Hydra.Sources.Kernel.Terms.Encoding        as Encoding
import qualified Hydra.Sources.Kernel.Terms.Environment     as Environment
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Hoisting        as Hoisting
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals

import qualified Hydra.Sources.Kernel.Lib.Chars             as LibChars
import qualified Hydra.Sources.Kernel.Lib.Defaults          as LibDefaults
import qualified Hydra.Sources.Kernel.Lib.Effects           as LibEffects
import qualified Hydra.Sources.Kernel.Lib.Eithers           as LibEithers
import qualified Hydra.Sources.Kernel.Lib.Equality          as LibEquality
import qualified Hydra.Sources.Kernel.Lib.Files             as LibFiles
import qualified Hydra.Sources.Kernel.Lib.Hashing           as LibHashing
import qualified Hydra.Sources.Kernel.Lib.Lists              as LibLists
import qualified Hydra.Sources.Kernel.Lib.Literals          as LibLiterals
import qualified Hydra.Sources.Kernel.Lib.Logic             as LibLogic
import qualified Hydra.Sources.Kernel.Lib.Maps              as LibMaps
import qualified Hydra.Sources.Kernel.Lib.Math              as LibMath
import qualified Hydra.Sources.Kernel.Lib.Optionals            as LibOptionals
import qualified Hydra.Sources.Kernel.Lib.Pairs             as LibPairs
import qualified Hydra.Sources.Kernel.Lib.Regex             as LibRegex
import qualified Hydra.Sources.Kernel.Lib.Sets              as LibSets
import qualified Hydra.Sources.Kernel.Lib.Strings           as LibStrings
import qualified Hydra.Sources.Kernel.Lib.System            as LibSystem
import qualified Hydra.Sources.Kernel.Lib.Text              as LibText
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
import qualified Hydra.Sources.Kernel.Terms.Parsers         as Parsers
import qualified Hydra.Sources.Kernel.Terms.Predicates     as Predicates
import qualified Hydra.Sources.Kernel.Terms.Reduction       as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect         as Reflect
import qualified Hydra.Sources.Kernel.Terms.Refs            as Refs
import qualified Hydra.Sources.Kernel.Terms.Resolution     as Resolution
import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Scoping         as Scoping
import qualified Hydra.Sources.Kernel.Terms.Serialization   as Serialization
import qualified Hydra.Sources.Kernel.Terms.Strip           as Strip
import qualified Hydra.Sources.Kernel.Terms.Parse.Regex      as ParseRegex
import qualified Hydra.Sources.Kernel.Terms.Print.Regex      as PrintRegex
import qualified Hydra.Sources.Kernel.Terms.Read.Docs        as ReadDocs
import qualified Hydra.Sources.Kernel.Terms.Show.Docs        as ShowDocs
import qualified Hydra.Sources.Kernel.Terms.Show.Paths      as ShowPaths
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Errors      as ShowError
import qualified Hydra.Sources.Kernel.Terms.Show.Error.Core as ShowErrorCore
import qualified Hydra.Sources.Kernel.Terms.Show.Error.Packaging as ShowErrorPackaging
import qualified Hydra.Sources.Kernel.Terms.Show.Graph      as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Variants       as ShowVariants
import qualified Hydra.Sources.Kernel.Terms.Show.Typing     as ShowTyping
import qualified Hydra.Sources.Kernel.Terms.Show.Util       as ShowUtil
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import qualified Hydra.Sources.Kernel.Terms.Validate.Core  as ValidateCore
import qualified Hydra.Sources.Kernel.Terms.Variables       as Variables
import qualified Hydra.Sources.Kernel.Terms.Validate.Packaging as ValidatePackaging


kernelTermsModules :: [Module]
kernelTermsModules = kernelPrimaryTermsModules

kernelPrimaryTermsModules :: [Module]
kernelPrimaryTermsModules = [
  Adapt.module_,
  Analysis.module_,
  Annotations.module_,
  Arity.module_,
  Checking.module_,
  Classes.module_,
  Generation.module_,
  Constants.module_,
  Decoding.module_,
  Dependencies.module_,
  Differentiation.module_,
  Dsls.module_,
  Encoding.module_,
  Environment.module_,
  ExtractCore.module_,
  ExtractUtil.module_,
  Formatting.module_,
  Hoisting.module_,
  Inference.module_,
  Languages.module_,
  Lexical.module_,
  LibChars.module_,
  LibDefaults.module_,
  LibEffects.module_,
  LibEithers.module_,
  LibEquality.module_,
  LibFiles.module_,
  LibHashing.module_,
  LibLists.module_,
  LibLiterals.module_,
  LibLogic.module_,
  LibMaps.module_,
  LibMath.module_,
  LibOptionals.module_,
  LibPairs.module_,
  LibRegex.module_,
  LibSets.module_,
  LibStrings.module_,
  LibSystem.module_,
  LibText.module_,
  Literals.module_,

  Names.module_,
  Parsers.module_,
  Predicates.module_,
  Reduction.module_,
  Reflect.module_,
  Refs.module_,
  Resolution.module_,
  Rewriting.module_,
  Scoping.module_,
  Serialization.module_,
  Strip.module_,
  ParseRegex.module_,
  PrintRegex.module_,
  ReadDocs.module_,
  ShowDocs.module_,
  ShowPaths.module_,
  ShowCore.module_,
  ShowError.module_,
  ShowErrorCore.module_,
  ShowErrorPackaging.module_,
  ShowGraph.module_,
  ShowVariants.module_,
  ShowTyping.module_,
  ShowUtil.module_,
  Sorting.module_,
  Substitution.module_,
  Templates.module_,
  Unification.module_,
  ValidateCore.module_,
  ValidatePackaging.module_,
  Variables.module_]

-- | The primitive-defining hydra.lib.* modules (a subset of kernelTermsModules).
-- These carry PrimitiveDefinitions only; they are the input to the term/primitive
-- DSL ref path (#467), which projects each primitive to a hydra.dsl.lib.<x> wrapper.
kernelLibModules :: [Module]
kernelLibModules = [
  LibChars.module_,
  LibDefaults.module_,
  LibEithers.module_,
  LibEquality.module_,
  LibLists.module_,
  LibLiterals.module_,
  LibLogic.module_,
  LibMaps.module_,
  LibMath.module_,
  LibOptionals.module_,
  LibPairs.module_,
  LibRegex.module_,
  LibSets.module_,
  LibStrings.module_]
