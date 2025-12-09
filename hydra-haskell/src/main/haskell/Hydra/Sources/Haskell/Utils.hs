module Hydra.Sources.Haskell.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import qualified Hydra.Sources.Kernel.Terms.All             as KernelTerms
import qualified Hydra.Sources.Kernel.Types.All             as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules   as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple    as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity           as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking        as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants       as Constants
import qualified Hydra.Sources.Kernel.Terms.Decode.Core     as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Encode.Core     as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Core    as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util    as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting      as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars        as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages       as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals        as Literals
import qualified Hydra.Sources.Kernel.Terms.Monads          as Monads
import qualified Hydra.Sources.Kernel.Terms.Names           as Names
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
import qualified Hydra.Sources.Kernel.Terms.Sorting         as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution    as Substitution
import qualified Hydra.Sources.Kernel.Terms.Tarjan          as Tarjan
import qualified Hydra.Sources.Kernel.Terms.Templates       as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification     as Unification
import           Prelude hiding ((++))
import qualified Data.Int                                   as I
import qualified Data.List                                  as L
import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified Data.Maybe                                 as Y

-- Additional imports
import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Sources.Haskell.Ast as HaskellAst
import qualified Hydra.Sources.Haskell.Language as HaskellLanguage
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting


type HaskellNamespaces = Namespaces H.ModuleName

haskellUtilsDefinition :: String -> TTerm a -> TBinding a
haskellUtilsDefinition = definitionInModule module_

module_ :: Module
module_ = Module ns elements
    [Formatting.module_, HaskellLanguage.module_, Schemas.module_, Names.module_]
    (HaskellAst.module_:KernelTypes.kernelTypesModules) $
    Just "Utilities for working with Haskell syntax trees"
  where
    ns = Namespace "hydra.ext.haskell.utils"
    elements = [
      toBinding applicationPattern,
      toBinding elementReference,
      toBinding hsapp,
      toBinding hslambda,
      toBinding hslit,
      toBinding hsvar,
      toBinding namespacesForModule,
      toBinding newtypeAccessorName,
      toBinding rawName,
      toBinding recordFieldReference,
      toBinding sanitizeHaskellName,
      toBinding simpleName,
      toBinding simpleValueBinding,
      toBinding toTypeApplication,
      toBinding typeNameForRecord,
      toBinding unionFieldReference,
      toBinding unpackForallType]

applicationPattern :: TBinding (H.Name -> [H.Pattern] -> H.Pattern)
applicationPattern = haskellUtilsDefinition "applicationPattern" $
  lambda "name" $ lambda "args" $
    inject H._Pattern H._Pattern_application $
      record H._ApplicationPattern [
        H._ApplicationPattern_name>>: var "name",
        H._ApplicationPattern_args>>: var "args"]

elementReference :: TBinding (HaskellNamespaces -> Name -> H.Name)
elementReference = haskellUtilsDefinition "elementReference" $
  lambda "namespaces" $ lambda "name" $ lets [
    "namespacePair">: Module.namespacesFocus $ var "namespaces",
    "gname">: Pairs.first $ var "namespacePair",
    "gmod">: unwrap H._ModuleName @@ (Pairs.second $ var "namespacePair"),
    "namespacesMap">: Module.namespacesMapping $ var "namespaces",
    "qname">: Names.qualifyName @@ var "name",
    "local">: Module.qualifiedNameLocal $ var "qname",
    "escLocal">: sanitizeHaskellName @@ var "local",
    "mns">: Module.qualifiedNameNamespace $ var "qname"] $
    Maybes.cases (Module.qualifiedNameNamespace $ var "qname")
      (simpleName @@ var "local") $
      lambda "ns" $
        Maybes.cases (Maps.lookup (var "ns") (var "namespacesMap"))
          (simpleName @@ var "local") $
          lambda "mn" $ lets [
            "aliasStr">: unwrap H._ModuleName @@ var "mn"] $
            Logic.ifElse (Equality.equal (var "ns") (var "gname"))
              (simpleName @@ var "escLocal")
              (rawName @@ (Strings.cat $ list [
                var "aliasStr",
                string ".",
                sanitizeHaskellName @@ var "local"]))

hsapp :: TBinding (H.Expression -> H.Expression -> H.Expression)
hsapp = haskellUtilsDefinition "hsapp" $
  lambda "l" $ lambda "r" $
    inject H._Expression H._Expression_application $
      record H._ApplicationExpression [
        H._ApplicationExpression_function>>: var "l",
        H._ApplicationExpression_argument>>: var "r"]

hslambda :: TBinding (H.Name -> H.Expression -> H.Expression)
hslambda = haskellUtilsDefinition "hslambda" $
  lambda "name" $ lambda "rhs" $
    inject H._Expression H._Expression_lambda $
      record H._LambdaExpression [
        H._LambdaExpression_bindings>>: list [inject H._Pattern H._Pattern_name $ var "name"],
        H._LambdaExpression_inner>>: var "rhs"]

hslit :: TBinding (H.Literal -> H.Expression)
hslit = haskellUtilsDefinition "hslit" $
  lambda "lit" $
    inject H._Expression H._Expression_literal $ var "lit"

hsvar :: TBinding (String -> H.Expression)
hsvar = haskellUtilsDefinition "hsvar" $
  lambda "s" $
    inject H._Expression H._Expression_variable $ (rawName @@ var "s")

namespacesForModule :: TBinding (Module -> Flow Graph HaskellNamespaces)
namespacesForModule = haskellUtilsDefinition "namespacesForModule" $
  lambda "mod" $
    bind "nss"
      (Schemas.moduleDependencyNamespaces @@ true @@ true @@ true @@ true @@ var "mod") $ lets [
    "ns">: Module.moduleNamespace $ var "mod",
    "focusPair">: var "toPair" @@ var "ns",
    "nssAsList">: Sets.toList $ var "nss",
    "nssPairs">: Lists.map (var "toPair") (var "nssAsList"),
    "emptyState">: pair Maps.empty Sets.empty,
    "finalState">: Lists.foldl (var "addPair") (var "emptyState") (var "nssPairs"),
    "resultMap">: Pairs.first $ var "finalState",
    "toModuleName">: lambda "namespace" $ lets [
      "namespaceStr">: unwrap _Namespace @@ var "namespace",
      "parts">: Strings.splitOn (string ".") (var "namespaceStr"),
      "lastPart">: Lists.last $ var "parts",
      "capitalized">: Formatting.capitalize @@ var "lastPart"] $
      wrap H._ModuleName $ var "capitalized",
    "toPair">: lambda "name" $
      pair (var "name") (var "toModuleName" @@ var "name"),
    "addPair">: lambda "state" $ lambda "namePair" $ lets [
      "currentMap">: Pairs.first $ var "state",
      "currentSet">: Pairs.second $ var "state",
      "name">: Pairs.first $ var "namePair",
      "alias">: Pairs.second $ var "namePair",
      "aliasStr">: unwrap H._ModuleName @@ var "alias"] $
      Logic.ifElse (Sets.member (var "alias") (var "currentSet"))
        (var "addPair" @@ var "state" @@ pair (var "name") (wrap H._ModuleName $ Strings.cat2 (var "aliasStr") (string "_")))
        (pair (Maps.insert (var "name") (var "alias") (var "currentMap")) (Sets.insert (var "alias") (var "currentSet")))] $
    Flows.pure $ Module.namespaces (var "focusPair") (var "resultMap")

newtypeAccessorName :: TBinding (Name -> String)
newtypeAccessorName = haskellUtilsDefinition "newtypeAccessorName" $
  lambda "name" $
    Strings.cat2 (string "un") (Names.localNameOf @@ var "name")

rawName :: TBinding (String -> H.Name)
rawName = haskellUtilsDefinition "rawName" $
  lambda "n" $
    inject H._Name H._Name_normal $
      record H._QualifiedName [
        H._QualifiedName_qualifiers>>: list ([] :: [TTerm H.NamePart]),
        H._QualifiedName_unqualified>>: wrap H._NamePart $ var "n"]

recordFieldReference :: TBinding (HaskellNamespaces -> Name -> Name -> H.Name)
recordFieldReference = haskellUtilsDefinition "recordFieldReference" $
  lambda "namespaces" $ lambda "sname" $ lambda "fname" $ lets [
    "fnameStr">: unwrap _Name @@ var "fname",
    "qname">: Names.qualifyName @@ var "sname",
    "ns">: Module.qualifiedNameNamespace $ var "qname",
    "typeNameStr">: typeNameForRecord @@ var "sname",
    "decapitalized">: Formatting.decapitalize @@ var "typeNameStr",
    "capitalized">: Formatting.capitalize @@ var "fnameStr",
    "nm">: Strings.cat2 (var "decapitalized") (var "capitalized"),
    "qualName">: record _QualifiedName [
      _QualifiedName_namespace>>: var "ns",
      _QualifiedName_local>>: var "nm"],
    "unqualName">: Names.unqualifyName @@ var "qualName"] $
    elementReference @@ var "namespaces" @@ var "unqualName"

sanitizeHaskellName :: TBinding (String -> String)
sanitizeHaskellName = haskellUtilsDefinition "sanitizeHaskellName" $
  Formatting.sanitizeWithUnderscores @@ (HaskellLanguage.reservedWords)

simpleName :: TBinding (String -> H.Name)
simpleName = haskellUtilsDefinition "simpleName" $
  compose (rawName) (sanitizeHaskellName)

simpleValueBinding :: TBinding (H.Name -> H.Expression -> Maybe H.LocalBindings -> H.ValueBinding)
simpleValueBinding = haskellUtilsDefinition "simpleValueBinding" $
  lambda "hname" $ lambda "rhs" $ lambda "bindings" $ lets [
    "pat">: inject H._Pattern H._Pattern_application $
      record H._ApplicationPattern [
        H._ApplicationPattern_name>>: var "hname",
        H._ApplicationPattern_args>>: list ([] :: [TTerm H.Pattern])],
    "rightHandSide">: wrap H._RightHandSide $ var "rhs"] $
    inject H._ValueBinding H._ValueBinding_simple $
      record H._SimpleValueBinding [
        H._SimpleValueBinding_pattern>>: var "pat",
        H._SimpleValueBinding_rhs>>: var "rightHandSide",
        H._SimpleValueBinding_localBindings>>: var "bindings"]

toTypeApplication :: TBinding ([H.Type] -> H.Type)
toTypeApplication = haskellUtilsDefinition "toTypeApplication" $
  lambda "types" $ lets [
    "app">: lambda "l" $
      Logic.ifElse (Equality.gt (Lists.length (var "l")) (int32 1))
        (inject H._Type H._Type_application $ record H._ApplicationType [
          H._ApplicationType_context>>: var "app" @@ (Lists.tail (var "l")),
          H._ApplicationType_argument>>: Lists.head (var "l")])
        (Lists.head $ var "l")] $
    var "app" @@ (Lists.reverse $ var "types")

typeNameForRecord :: TBinding (Name -> String)
typeNameForRecord = haskellUtilsDefinition "typeNameForRecord" $
  lambda "sname" $ lets [
    "snameStr">: Core.unName $ var "sname",
    "parts">: Strings.splitOn (string ".") (var "snameStr")] $
    Lists.last $ var "parts"

unionFieldReference :: TBinding (HaskellNamespaces -> Name -> Name -> H.Name)
unionFieldReference = haskellUtilsDefinition "unionFieldReference" $
  lambda "namespaces" $ lambda "sname" $ lambda "fname" $ lets [
    "fnameStr">: unwrap _Name @@ var "fname",
    "qname">: Names.qualifyName @@ var "sname",
    "ns">: Module.qualifiedNameNamespace $ var "qname",
    "typeNameStr">: typeNameForRecord @@ var "sname",
    "capitalizedTypeName">: Formatting.capitalize @@ var "typeNameStr",
    "capitalizedFieldName">: Formatting.capitalize @@ var "fnameStr",
    "nm">: Strings.cat2 (var "capitalizedTypeName") (var "capitalizedFieldName"),
    "qualName">: record _QualifiedName [
      _QualifiedName_namespace>>: var "ns",
      _QualifiedName_local>>: var "nm"],
    "unqualName">: Names.unqualifyName @@ var "qualName"] $
    elementReference @@ var "namespaces" @@ var "unqualName"

unpackForallType :: TBinding (Graph -> Type -> ([Name], Type))
unpackForallType = haskellUtilsDefinition "unpackForallType" $
  lambdas ["cx", "t"] $ cases _Type (Rewriting.deannotateType @@ var "t")
    (Just $ pair (list ([] :: [TTerm Name])) (var "t")) [
    _Type_forall>>: lambda "fat" $ lets [
      "v">: Core.forallTypeParameter $ var "fat",
      "tbody">: Core.forallTypeBody $ var "fat",
      "recursiveResult">: unpackForallType @@ var "cx" @@ var "tbody",
      "vars">: Pairs.first $ var "recursiveResult",
      "finalType">: Pairs.second $ var "recursiveResult"] $
      pair (Lists.cons (var "v") (var "vars")) (var "finalType")]
