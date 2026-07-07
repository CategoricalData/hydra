-- Note: this is an automatically generated file. Do not edit.

-- | Functions for working with qualified names.

module Hydra.Names where

import qualified Hydra.Annotations as Annotations
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Overlay.Haskell.Lib.Equality as Equality
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Literals as Literals
import qualified Hydra.Overlay.Haskell.Lib.Logic as Logic
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Math as Math
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Overlay.Haskell.Lib.Sets as Sets
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S

-- | Pick a string label that does not collide with a reserved set, by appending a numeric suffix when necessary
chooseUniqueLabel :: S.Set String -> String -> String
chooseUniqueLabel reserved label =

      let tryLabel =
              \index ->
                let candidate = Logic.ifElse (Equality.equal index 1) label (Strings.cat2 label (Literals.showInt32 index))
                in (Logic.ifElse (Sets.member candidate reserved) (tryLabel (Math.add index 1)) candidate)
      in (tryLabel 1)

-- | Given a mapping of namespaces to prefixes, convert a name to a compact string representation
compactName :: M.Map Packaging.ModuleName String -> Core.Name -> String
compactName namespaces name =

      let qualName = qualifyName name
          mns = Util.qualifiedNameModuleName qualName
          local = Util.qualifiedNameLocal qualName
      in (Optionals.cases mns (Core.unName name) (\ns -> Optionals.cases (Maps.lookup ns namespaces) local (\pre -> Strings.cat [
        pre,
        ":",
        local])))

-- | Generate a DSL module name from a source module name
dslModuleName :: Packaging.ModuleName -> Packaging.ModuleName
dslModuleName ns =

      let parts = Strings.splitOn "." (Packaging.unModuleName ns)
          prefixFull =
                  Packaging.ModuleName (Strings.cat [
                    "hydra.dsl.",
                    (Packaging.unModuleName ns)])
      in (Optionals.cases (Lists.uncons parts) prefixFull (\ht -> Logic.ifElse (Equality.equal (Pairs.first ht) "hydra") (Packaging.ModuleName (Strings.cat [
        "hydra.dsl.",
        (Strings.intercalate "." (Pairs.second ht))])) prefixFull))

-- | Generate a fresh type variable name, threading InferenceContext
freshName :: Typing.InferenceContext -> (Core.Name, Typing.InferenceContext)
freshName cx =

      let count = Typing.inferenceContextFreshTypeVariableCount cx
      in (
        normalTypeVariable count,
        Typing.InferenceContext {
          Typing.inferenceContextFreshTypeVariableCount = (Math.add count 1),
          Typing.inferenceContextTrace = (Typing.inferenceContextTrace cx)})

-- | Generate multiple fresh type variable names, threading InferenceContext
freshNames :: Int -> Typing.InferenceContext -> ([Core.Name], Typing.InferenceContext)
freshNames n cx =

      let go =
              \acc -> \_ ->
                let names = Pairs.first acc
                    cx0 = Pairs.second acc
                    result = freshName cx0
                    name = Pairs.first result
                    cx1 = Pairs.second result
                in (Lists.concat2 names (Lists.pure name), cx1)
      in (Lists.foldl go ([], cx) (Lists.replicate n ()))

-- | Extract the local part of a name
localNameOf :: Core.Name -> String
localNameOf arg_ = Util.qualifiedNameLocal (qualifyName arg_)

-- | Extract the module name of a name, if any
moduleNameOf :: Core.Name -> Maybe Packaging.ModuleName
moduleNameOf arg_ = Util.qualifiedNameModuleName (qualifyName arg_)

-- | Convert a module name to a file path with the given case convention and file extension
moduleNameToFilePath :: Util.CaseConvention -> File.FileExtension -> Packaging.ModuleName -> String
moduleNameToFilePath caseConv ext ns =

      let parts = Lists.map (Formatting.convertCase Util.CaseConventionCamel caseConv) (Strings.splitOn "." (Packaging.unModuleName ns))
      in (Strings.cat2 (Strings.cat2 (Strings.intercalate "/" parts) ".") (File.unFileExtension ext))

-- | Convert a name to file path, given case conventions for namespaces and local names, and assuming '/' as the file path separator
nameToFilePath :: Util.CaseConvention -> Util.CaseConvention -> File.FileExtension -> Core.Name -> String
nameToFilePath nsConv localConv ext name =

      let qualName = qualifyName name
          ns = Util.qualifiedNameModuleName qualName
          local = Util.qualifiedNameLocal qualName
          nsToFilePath =
                  \nsArg -> Strings.intercalate "/" (Lists.map (\part -> Formatting.convertCase Util.CaseConventionCamel nsConv part) (Strings.splitOn "." (Packaging.unModuleName nsArg)))
          prefix = Optionals.cases ns "" (\n -> Strings.cat2 (nsToFilePath n) "/")
          suffix = Formatting.convertCase Util.CaseConventionPascal localConv local
      in (Strings.cat [
        prefix,
        suffix,
        ".",
        (File.unFileExtension ext)])

-- | Type variable naming convention follows Haskell: t0, t1, etc.
normalTypeVariable :: Int -> Core.Name
normalTypeVariable i = Core.Name (Strings.cat2 "t" (Literals.showInt32 i))

-- | Prepend a SubtermStep to the InferenceContext's trace. The trace is accumulated backwards as inference descends through subterms; at error-emission time the list is reversed and wrapped into a SubtermPath stamped onto the error.
pushSubtermStep :: Paths.SubtermStep -> Typing.InferenceContext -> Typing.InferenceContext
pushSubtermStep step cx =
    Typing.InferenceContext {
      Typing.inferenceContextFreshTypeVariableCount = (Typing.inferenceContextFreshTypeVariableCount cx),
      Typing.inferenceContextTrace = (Lists.cons step (Typing.inferenceContextTrace cx))}

-- | Construct a qualified (dot-separated) name
qname :: Packaging.ModuleName -> String -> Core.Name
qname ns name =
    Core.Name (Strings.cat [
      Packaging.unModuleName ns,
      ".",
      name])

-- | Split a dot-separated name into a namespace and local name
qualifyName :: Core.Name -> Util.QualifiedName
qualifyName name =

      let parts = Lists.reverse (Strings.splitOn "." (Core.unName name))
      in (Optionals.cases (Lists.uncons parts) (Util.QualifiedName {
        Util.qualifiedNameModuleName = Nothing,
        Util.qualifiedNameLocal = (Core.unName name)}) (\uc ->
        let localName = Pairs.first uc
            restReversed = Pairs.second uc
        in (Logic.ifElse (Lists.null restReversed) (Util.QualifiedName {
          Util.qualifiedNameModuleName = Nothing,
          Util.qualifiedNameLocal = (Core.unName name)}) (Util.QualifiedName {
          Util.qualifiedNameModuleName = (Just (Packaging.ModuleName (Strings.intercalate "." (Lists.reverse restReversed)))),
          Util.qualifiedNameLocal = localName}))))

-- | Restore the original trace from baseCx, while keeping the freshTypeVariableCount from newCx. Used between sibling sub-inferences (e.g. application LHS vs RHS) so that an error in the second sibling doesn't include the first sibling's trace path. Returns a new InferenceContext.
restoreTrace :: Typing.InferenceContext -> Typing.InferenceContext -> Typing.InferenceContext
restoreTrace baseCx newCx =
    Typing.InferenceContext {
      Typing.inferenceContextFreshTypeVariableCount = (Typing.inferenceContextFreshTypeVariableCount newCx),
      Typing.inferenceContextTrace = (Typing.inferenceContextTrace baseCx)}

-- | Convert a qualified name to a dot-separated name
unqualifyName :: Util.QualifiedName -> Core.Name
unqualifyName qname =

      let prefix = Optionals.cases (Util.qualifiedNameModuleName qname) "" (\n -> Strings.cat2 (Packaging.unModuleName n) ".")
      in (Core.Name (Strings.cat2 prefix (Util.qualifiedNameLocal qname)))
