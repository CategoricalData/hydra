-- Note: this is an automatically generated file. Do not edit.
-- | Functions for working with qualified names.

module Hydra.Names where
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Literals as Literals
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
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
import qualified Data.Map as M
import qualified Data.Set as S
-- | Given a mapping of namespaces to prefixes, convert a name to a compact string representation
compactName :: M.Map Packaging.ModuleName String -> Core.Name -> String
compactName namespaces name =

      let qualName = qualifyName name
          mns = Packaging.qualifiedNameModuleName qualName
          local = Packaging.qualifiedNameLocal qualName
      in (Maybes.maybe (Core.unName name) (\ns -> Maybes.maybe local (\pre -> Strings.cat [
        pre,
        ":",
        local]) (Maps.lookup ns namespaces)) mns)
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
localNameOf arg_ = Packaging.qualifiedNameLocal (qualifyName arg_)
-- | Extract the module name of a name, if any
moduleNameOf :: Core.Name -> Maybe Packaging.ModuleName
moduleNameOf arg_ = Packaging.qualifiedNameModuleName (qualifyName arg_)
-- | Convert a module name to a file path with the given case convention and file extension
moduleNameToFilePath :: Util.CaseConvention -> Packaging.FileExtension -> Packaging.ModuleName -> String
moduleNameToFilePath caseConv ext ns =

      let parts = Lists.map (Formatting.convertCase Util.CaseConventionCamel caseConv) (Strings.splitOn "." (Packaging.unModuleName ns))
      in (Strings.cat2 (Strings.cat2 (Strings.intercalate "/" parts) ".") (Packaging.unFileExtension ext))
-- | Convert a name to file path, given case conventions for namespaces and local names, and assuming '/' as the file path separator
nameToFilePath :: Util.CaseConvention -> Util.CaseConvention -> Packaging.FileExtension -> Core.Name -> String
nameToFilePath nsConv localConv ext name =

      let qualName = qualifyName name
          ns = Packaging.qualifiedNameModuleName qualName
          local = Packaging.qualifiedNameLocal qualName
          nsToFilePath =
                  \nsArg -> Strings.intercalate "/" (Lists.map (\part -> Formatting.convertCase Util.CaseConventionCamel nsConv part) (Strings.splitOn "." (Packaging.unModuleName nsArg)))
          prefix = Maybes.maybe "" (\n -> Strings.cat2 (nsToFilePath n) "/") ns
          suffix = Formatting.convertCase Util.CaseConventionPascal localConv local
      in (Strings.cat [
        prefix,
        suffix,
        ".",
        (Packaging.unFileExtension ext)])
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
qualifyName :: Core.Name -> Packaging.QualifiedName
qualifyName name =

      let parts = Lists.reverse (Strings.splitOn "." (Core.unName name))
      in (Maybes.maybe (Packaging.QualifiedName {
        Packaging.qualifiedNameModuleName = Nothing,
        Packaging.qualifiedNameLocal = (Core.unName name)}) (\uc ->
        let localName = Pairs.first uc
            restReversed = Pairs.second uc
        in (Logic.ifElse (Lists.null restReversed) (Packaging.QualifiedName {
          Packaging.qualifiedNameModuleName = Nothing,
          Packaging.qualifiedNameLocal = (Core.unName name)}) (Packaging.QualifiedName {
          Packaging.qualifiedNameModuleName = (Just (Packaging.ModuleName (Strings.intercalate "." (Lists.reverse restReversed)))),
          Packaging.qualifiedNameLocal = localName}))) (Lists.uncons parts))
-- | Restore the original trace from baseCx, while keeping the freshTypeVariableCount from newCx. Used between sibling sub-inferences (e.g. application LHS vs RHS) so that an error in the second sibling doesn't include the first sibling's trace path. Returns a new InferenceContext.
restoreTrace :: Typing.InferenceContext -> Typing.InferenceContext -> Typing.InferenceContext
restoreTrace baseCx newCx =
    Typing.InferenceContext {
      Typing.inferenceContextFreshTypeVariableCount = (Typing.inferenceContextFreshTypeVariableCount newCx),
      Typing.inferenceContextTrace = (Typing.inferenceContextTrace baseCx)}
-- | Generate a unique label by appending a suffix if the label is already in use
uniqueLabel :: S.Set String -> String -> String
uniqueLabel visited l = Logic.ifElse (Sets.member l visited) (uniqueLabel visited (Strings.cat2 l "'")) l
-- | Convert a qualified name to a dot-separated name
unqualifyName :: Packaging.QualifiedName -> Core.Name
unqualifyName qname =

      let prefix = Maybes.maybe "" (\n -> Strings.cat2 (Packaging.unModuleName n) ".") (Packaging.qualifiedNameModuleName qname)
      in (Core.Name (Strings.cat2 prefix (Packaging.qualifiedNameLocal qname)))
