module Hydra.Sources.Haskell.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta          as Meta
import qualified Hydra.Dsl.Module        as Module
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Testing       as Testing
import qualified Hydra.Dsl.TBase         as TBase
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Dsl.Util          as Util
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
import qualified Hydra.Sources.Kernel.Terms.Describe.Core   as DescribeCore
import qualified Hydra.Sources.Kernel.Terms.Describe.Util   as DescribeUtil
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
import qualified Hydra.Sources.Kernel.Terms.Variants        as Variants
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
haskellUtilsDefinition = definitionInModule haskellUtilsModule

haskellUtilsModule :: Module
haskellUtilsModule = Module ns elements
    [Formatting.module_, HaskellLanguage.haskellLanguageModule, Schemas.module_, Names.module_]
    (HaskellAst.haskellAstModule:KernelTypes.kernelTypesModules) $
    Just "Utilities for working with Haskell syntax trees"
  where
    ns = Namespace "hydra.ext.haskell.utils"
    elements = [
      el applicationPatternDef,
      el elementReferenceDef,
      el hsappDef,
      el hslambdaDef,
      el hslitDef,
      el hsvarDef,
      el namespacesForModuleDef,
      el newtypeAccessorNameDef,
      el rawNameDef,
      el recordFieldReferenceDef,
      el sanitizeHaskellNameDef,
      el simpleNameDef,
      el simpleValueBindingDef,
      el toTypeApplicationDef,
      el typeNameForRecordDef,
      el unionFieldReferenceDef,
      el unpackForallTypeDef]

applicationPatternDef :: TBinding (H.Name -> [H.Pattern] -> H.Pattern)
applicationPatternDef = haskellUtilsDefinition "applicationPattern" $
  lambda "name" $ lambda "args" $
    inject H._Pattern H._Pattern_application $
      record H._ApplicationPattern [
        H._ApplicationPattern_name>>: var "name",
        H._ApplicationPattern_args>>: var "args"]

elementReferenceDef :: TBinding (HaskellNamespaces -> Name -> H.Name)
elementReferenceDef = haskellUtilsDefinition "elementReference" $
  lambda "namespaces" $ lambda "name" $ lets [
    "namespacePair">: Module.namespacesFocus $ var "namespaces",
    "gname">: first $ var "namespacePair",
    "gmod">: unwrap H._ModuleName @@ (second $ var "namespacePair"),
    "namespacesMap">: Module.namespacesMapping $ var "namespaces",
    "qname">: ref Names.qualifyNameDef @@ var "name",
    "local">: Module.qualifiedNameLocal $ var "qname",
    "escLocal">: ref sanitizeHaskellNameDef @@ var "local",
    "mns">: Module.qualifiedNameNamespace $ var "qname"] $
    Maybes.cases (Module.qualifiedNameNamespace $ var "qname")
      (ref simpleNameDef @@ var "local") $
      lambda "ns" $
        Maybes.cases (Maps.lookup (var "ns") (var "namespacesMap"))
          (ref simpleNameDef @@ var "local") $
          lambda "mn" $ lets [
            "aliasStr">: unwrap H._ModuleName @@ var "mn"] $
            Logic.ifElse (Equality.equal (var "ns") (var "gname"))
              (ref simpleNameDef @@ var "escLocal")
              (ref rawNameDef @@ (Strings.cat $ list [
                var "aliasStr",
                string ".",
                ref sanitizeHaskellNameDef @@ var "local"]))

hsappDef :: TBinding (H.Expression -> H.Expression -> H.Expression)
hsappDef = haskellUtilsDefinition "hsapp" $
  lambda "l" $ lambda "r" $
    inject H._Expression H._Expression_application $
      record H._ApplicationExpression [
        H._ApplicationExpression_function>>: var "l",
        H._ApplicationExpression_argument>>: var "r"]

hslambdaDef :: TBinding (H.Name -> H.Expression -> H.Expression)
hslambdaDef = haskellUtilsDefinition "hslambda" $
  lambda "name" $ lambda "rhs" $
    inject H._Expression H._Expression_lambda $
      record H._LambdaExpression [
        H._LambdaExpression_bindings>>: list [inject H._Pattern H._Pattern_name $ var "name"],
        H._LambdaExpression_inner>>: var "rhs"]

hslitDef :: TBinding (H.Literal -> H.Expression)
hslitDef = haskellUtilsDefinition "hslit" $
  lambda "lit" $
    inject H._Expression H._Expression_literal $ var "lit"

hsvarDef :: TBinding (String -> H.Expression)
hsvarDef = haskellUtilsDefinition "hsvar" $
  lambda "s" $
    inject H._Expression H._Expression_variable $ (ref rawNameDef @@ var "s")

namespacesForModuleDef :: TBinding (Module -> Flow Graph HaskellNamespaces)
namespacesForModuleDef = haskellUtilsDefinition "namespacesForModule" $
  lambda "mod" $
    bind "nss"
      (ref Schemas.moduleDependencyNamespacesDef @@ true @@ true @@ true @@ true @@ var "mod") $ lets [
    "ns">: Module.moduleNamespace $ var "mod",
    "focusPair">: var "toPair" @@ var "ns",
    "nssAsList">: Sets.toList $ var "nss",
    "nssPairs">: Lists.map (var "toPair") (var "nssAsList"),
    "emptyState">: tuple2 Maps.empty Sets.empty,
    "finalState">: Lists.foldl (var "addPair") (var "emptyState") (var "nssPairs"),
    "resultMap">: first $ var "finalState",
    "toModuleName">: lambda "namespace" $ lets [
      "namespaceStr">: unwrap _Namespace @@ var "namespace",
      "parts">: Strings.splitOn (string ".") (var "namespaceStr"),
      "lastPart">: Lists.last $ var "parts",
      "capitalized">: ref Formatting.capitalizeDef @@ var "lastPart"] $
      wrap H._ModuleName $ var "capitalized",
    "toPair">: lambda "name" $
      tuple2 (var "name") (var "toModuleName" @@ var "name"),
    "addPair">: lambda "state" $ lambda "namePair" $ lets [
      "currentMap">: first $ var "state",
      "currentSet">: second $ var "state",
      "name">: first $ var "namePair",
      "alias">: second $ var "namePair",
      "aliasStr">: unwrap H._ModuleName @@ var "alias"] $
      Logic.ifElse (Sets.member (var "alias") (var "currentSet"))
        (var "addPair" @@ var "state" @@ tuple2 (var "name") (wrap H._ModuleName $ Strings.cat2 (var "aliasStr") (string "_")))
        (tuple2 (Maps.insert (var "name") (var "alias") (var "currentMap")) (Sets.insert (var "alias") (var "currentSet")))] $
    Flows.pure $ Module.namespaces (var "focusPair") (var "resultMap")

newtypeAccessorNameDef :: TBinding (Name -> String)
newtypeAccessorNameDef = haskellUtilsDefinition "newtypeAccessorName" $
  lambda "name" $
    Strings.cat2 (string "un") (ref Names.localNameOfDef @@ var "name")

rawNameDef :: TBinding (String -> H.Name)
rawNameDef = haskellUtilsDefinition "rawName" $
  lambda "n" $
    inject H._Name H._Name_normal $
      record H._QualifiedName [
        H._QualifiedName_qualifiers>>: list [],
        H._QualifiedName_unqualified>>: wrap H._NamePart $ var "n"]

recordFieldReferenceDef :: TBinding (HaskellNamespaces -> Name -> Name -> H.Name)
recordFieldReferenceDef = haskellUtilsDefinition "recordFieldReference" $
  lambda "namespaces" $ lambda "sname" $ lambda "fname" $ lets [
    "fnameStr">: unwrap _Name @@ var "fname",
    "qname">: ref Names.qualifyNameDef @@ var "sname",
    "ns">: Module.qualifiedNameNamespace $ var "qname",
    "typeNameStr">: ref typeNameForRecordDef @@ var "sname",
    "decapitalized">: ref Formatting.decapitalizeDef @@ var "typeNameStr",
    "capitalized">: ref Formatting.capitalizeDef @@ var "fnameStr",
    "nm">: Strings.cat2 (var "decapitalized") (var "capitalized"),
    "qualName">: record _QualifiedName [
      _QualifiedName_namespace>>: var "ns",
      _QualifiedName_local>>: var "nm"],
    "unqualName">: ref Names.unqualifyNameDef @@ var "qualName"] $
    ref elementReferenceDef @@ var "namespaces" @@ var "unqualName"

sanitizeHaskellNameDef :: TBinding (String -> String)
sanitizeHaskellNameDef = haskellUtilsDefinition "sanitizeHaskellName" $
  ref Formatting.sanitizeWithUnderscoresDef @@ (ref HaskellLanguage.reservedWordsDef)

simpleNameDef :: TBinding (String -> H.Name)
simpleNameDef = haskellUtilsDefinition "simpleName" $
  compose (ref rawNameDef) (ref sanitizeHaskellNameDef)

simpleValueBindingDef :: TBinding (H.Name -> H.Expression -> Maybe H.LocalBindings -> H.ValueBinding)
simpleValueBindingDef = haskellUtilsDefinition "simpleValueBinding" $
  lambda "hname" $ lambda "rhs" $ lambda "bindings" $ lets [
    "pat">: inject H._Pattern H._Pattern_application $
      record H._ApplicationPattern [
        H._ApplicationPattern_name>>: var "hname",
        H._ApplicationPattern_args>>: list []],
    "rightHandSide">: wrap H._RightHandSide $ var "rhs"] $
    inject H._ValueBinding H._ValueBinding_simple $
      record H._SimpleValueBinding [
        H._SimpleValueBinding_pattern>>: var "pat",
        H._SimpleValueBinding_rhs>>: var "rightHandSide",
        H._SimpleValueBinding_localBindings>>: var "bindings"]

toTypeApplicationDef :: TBinding ([H.Type] -> H.Type)
toTypeApplicationDef = haskellUtilsDefinition "toTypeApplication" $
  lambda "types" $ lets [
    "app">: lambda "l" $
      Logic.ifElse (Equality.gt (Lists.length (var "l")) (int32 1))
        (inject H._Type H._Type_application $ record H._ApplicationType [
          H._ApplicationType_context>>: var "app" @@ (Lists.tail (var "l")),
          H._ApplicationType_argument>>: Lists.head (var "l")])
        (Lists.head $ var "l")] $
    var "app" @@ (Lists.reverse $ var "types")

typeNameForRecordDef :: TBinding (Name -> String)
typeNameForRecordDef = haskellUtilsDefinition "typeNameForRecord" $
  lambda "sname" $ lets [
    "snameStr">: Core.unName $ var "sname",
    "parts">: Strings.splitOn (string ".") (var "snameStr")] $
    Lists.last $ var "parts"

unionFieldReferenceDef :: TBinding (HaskellNamespaces -> Name -> Name -> H.Name)
unionFieldReferenceDef = haskellUtilsDefinition "unionFieldReference" $
  lambda "namespaces" $ lambda "sname" $ lambda "fname" $ lets [
    "fnameStr">: unwrap _Name @@ var "fname",
    "qname">: ref Names.qualifyNameDef @@ var "sname",
    "ns">: Module.qualifiedNameNamespace $ var "qname",
    "typeNameStr">: ref typeNameForRecordDef @@ var "sname",
    "capitalizedTypeName">: ref Formatting.capitalizeDef @@ var "typeNameStr",
    "capitalizedFieldName">: ref Formatting.capitalizeDef @@ var "fnameStr",
    "nm">: Strings.cat2 (var "capitalizedTypeName") (var "capitalizedFieldName"),
    "qualName">: record _QualifiedName [
      _QualifiedName_namespace>>: var "ns",
      _QualifiedName_local>>: var "nm"],
    "unqualName">: ref Names.unqualifyNameDef @@ var "qualName"] $
    ref elementReferenceDef @@ var "namespaces" @@ var "unqualName"

unpackForallTypeDef :: TBinding (Graph -> Type -> ([Name], Type))
unpackForallTypeDef = haskellUtilsDefinition "unpackForallType" $
  lambdas ["cx", "t"] $ cases _Type (ref Rewriting.deannotateTypeDef @@ var "t")
    (Just $ tuple2 (list []) (var "t")) [
    _Type_forall>>: lambda "fat" $ lets [
      "v">: Core.forallTypeParameter $ var "fat",
      "tbody">: Core.forallTypeBody $ var "fat",
      "recursiveResult">: ref unpackForallTypeDef @@ var "cx" @@ var "tbody",
      "vars">: first $ var "recursiveResult",
      "finalType">: second $ var "recursiveResult"] $
      tuple2 (Lists.cons (var "v") (var "vars")) (var "finalType")]
