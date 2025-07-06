module Hydra.Sources.Tier3.Ext.Haskell.Utils where

-- Standard Tier-3 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Compute                    as Compute
import qualified Hydra.Dsl.Core                       as Core
import qualified Hydra.Dsl.Graph                      as Graph
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals              as Optionals
import           Hydra.Dsl.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Lib.Strings                as Strings
import qualified Hydra.Dsl.Mantle                     as Mantle
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.TTerms                     as TTerms
import qualified Hydra.Dsl.TTypes                     as TTypes
import qualified Hydra.Dsl.Terms                      as Terms
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Types                      as Types
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Sources.Tier1.All              as Tier1
import qualified Hydra.Sources.Tier1.Constants        as Constants
import qualified Hydra.Sources.Tier1.Decode           as Decode
import qualified Hydra.Sources.Tier1.Encode.Core      as EncodeCore
import qualified Hydra.Sources.Tier1.Formatting       as Formatting
import qualified Hydra.Sources.Tier1.Literals         as Literals
import qualified Hydra.Sources.Tier1.Strip            as Strip
import qualified Hydra.Sources.Tier2.Adapt.Utils     as AdaptUtils
import qualified Hydra.Sources.Tier2.Adapt.Modules         as AdaptModules
import qualified Hydra.Sources.Tier2.Annotations      as Annotations
import qualified Hydra.Sources.Tier2.Arity            as Arity
import qualified Hydra.Sources.Tier2.Languages     as Languages
import qualified Hydra.Sources.Tier2.Decode.Core      as DecodeCore
import qualified Hydra.Sources.Tier2.Describe.Core    as DescribeCore
import qualified Hydra.Sources.Tier2.Extract.Core     as ExtractCore
import qualified Hydra.Sources.Tier2.Monads           as Monads
import qualified Hydra.Sources.Tier2.Grammars  as Grammars
import qualified Hydra.Sources.Tier2.Inference        as Inference
import qualified Hydra.Sources.Tier2.Lexical          as Lexical
import qualified Hydra.Sources.Tier2.Adapt.Literals  as AdaptLiterals
import qualified Hydra.Sources.Tier2.Qnames           as Qnames
import qualified Hydra.Sources.Tier2.Reduction        as Reduction
import qualified Hydra.Sources.Tier2.Rewriting        as Rewriting
import qualified Hydra.Sources.Tier2.Schemas          as Schemas
import qualified Hydra.Sources.Tier2.Serialization    as Serialization
import qualified Hydra.Sources.Tier2.Show.Accessors   as ShowAccessors
import qualified Hydra.Sources.Tier2.Show.Core        as ShowCore
import qualified Hydra.Sources.Tier2.Sorting          as Sorting
import qualified Hydra.Sources.Tier2.Substitution     as Substitution
import qualified Hydra.Sources.Tier2.Tarjan           as Tarjan
import qualified Hydra.Sources.Tier2.Templating       as Templating
import qualified Hydra.Sources.Tier2.Adapt.Terms     as AdaptTerms
import qualified Hydra.Sources.Tier2.Unification      as Unification
import qualified Hydra.Sources.Tier2.Variants         as Variants
import qualified Data.Int                             as I
import qualified Data.List                            as L
import qualified Data.Map                             as M
import qualified Data.Set                             as S
import qualified Data.Maybe                           as Y

import qualified Hydra.Ext.Haskell.Ast as H
import qualified Hydra.Sources.Tier3.Ext.Haskell.Ast as HaskellAst
import qualified Hydra.Sources.Tier3.Ext.Haskell.Language as HaskellLanguage


type HaskellNamespaces = Namespaces H.ModuleName

haskellUtilsDefinition :: String -> TTerm a -> TElement a
haskellUtilsDefinition = definitionInModule haskellUtilsModule

haskellUtilsModule :: Module
haskellUtilsModule = Module ns elements
    [Tier1.hydraFormattingModule, HaskellLanguage.haskellLanguageModule, Schemas.hydraSchemasModule, Qnames.hydraQnamesModule]
    [Tier1.hydraCodersModule, Tier1.hydraModuleModule, Tier1.hydraTopologyModule, Tier1.hydraTypingModule, HaskellAst.haskellAstModule] $
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

applicationPatternDef :: TElement (H.Name -> [H.Pattern] -> H.Pattern)
applicationPatternDef = haskellUtilsDefinition "applicationPattern" $
  lambda "name" $ lambda "args" $
    inject H._Pattern H._Pattern_application $
      record H._ApplicationPattern [
        H._ApplicationPattern_name>>: var "name",
        H._ApplicationPattern_args>>: var "args"]

elementReferenceDef :: TElement (HaskellNamespaces -> Name -> H.Name)
elementReferenceDef = haskellUtilsDefinition "elementReference" $
  lambda "namespaces" $ lambda "name" $ lets [
    "namespacePair">: Module.namespacesFocus $ var "namespaces",
    "gname">: first $ var "namespacePair",
    "gmod">: unwrap H._ModuleName @@ (second $ var "namespacePair"),
    "namespacesMap">: Module.namespacesMapping $ var "namespaces",
    "qname">: ref Qnames.qualifyNameDef @@ var "name",
    "local">: Module.qualifiedNameLocal $ var "qname",
    "escLocal">: ref sanitizeHaskellNameDef @@ var "local",
    "mns">: Module.qualifiedNameNamespace $ var "qname"] $
    Optionals.cases (Module.qualifiedNameNamespace $ var "qname")
      (ref simpleNameDef @@ var "local") $
      lambda "ns" $
        Optionals.cases (Maps.lookup (var "ns") (var "namespacesMap"))
          (ref simpleNameDef @@ var "local") $
          lambda "mn" $ lets [
            "aliasStr">: unwrap H._ModuleName @@ var "mn"] $
            Logic.ifElse (Equality.equal (var "ns") (var "gname"))
              (ref simpleNameDef @@ var "escLocal")
              (ref rawNameDef @@ (Strings.cat $ list [
                var "aliasStr",
                string ".",
                ref sanitizeHaskellNameDef @@ var "local"]))

hsappDef :: TElement (H.Expression -> H.Expression -> H.Expression)
hsappDef = haskellUtilsDefinition "hsapp" $
  lambda "l" $ lambda "r" $
    inject H._Expression H._Expression_application $
      record H._ApplicationExpression [
        H._ApplicationExpression_function>>: var "l",
        H._ApplicationExpression_argument>>: var "r"]

hslambdaDef :: TElement (H.Name -> H.Expression -> H.Expression)
hslambdaDef = haskellUtilsDefinition "hslambda" $
  lambda "name" $ lambda "rhs" $
    inject H._Expression H._Expression_lambda $
      record H._LambdaExpression [
        H._LambdaExpression_bindings>>: list [inject H._Pattern H._Pattern_name $ var "name"],
        H._LambdaExpression_inner>>: var "rhs"]

hslitDef :: TElement (H.Literal -> H.Expression)
hslitDef = haskellUtilsDefinition "hslit" $
  lambda "lit" $
    inject H._Expression H._Expression_literal $ var "lit"

hsvarDef :: TElement (String -> H.Expression)
hsvarDef = haskellUtilsDefinition "hsvar" $
  lambda "s" $
    inject H._Expression H._Expression_variable $ (ref rawNameDef @@ var "s")

namespacesForModuleDef :: TElement (Module -> Flow Graph HaskellNamespaces)
namespacesForModuleDef = haskellUtilsDefinition "namespacesForModule" $
  lambda "mod" $
    withVar "nss"
      (ref Schemas.moduleDependencyNamespacesDef @@ true @@ true @@ true @@ true @@ var "mod") $ lets [
    "ns">: Module.moduleNamespace $ var "mod",
    "focusPair">: var "toPair" @@ var "ns",
    "nssAsList">: Sets.toList $ var "nss",
    "nssPairs">: Lists.map (var "toPair") (var "nssAsList"),
    "emptyState">: pair Maps.empty Sets.empty,
    "finalState">: Lists.foldl (var "addPair") (var "emptyState") (var "nssPairs"),
    "resultMap">: first $ var "finalState",
    "toModuleName">: lambda "namespace" $ lets [
      "namespaceStr">: unwrap _Namespace @@ var "namespace",
      "parts">: Strings.splitOn (string ".") (var "namespaceStr"),
      "lastPart">: Lists.last $ var "parts",
      "capitalized">: ref Formatting.capitalizeDef @@ var "lastPart"] $
      wrap H._ModuleName $ var "capitalized",
    "toPair">: lambda "name" $
      pair (var "name") (var "toModuleName" @@ var "name"),
    "addPair">: lambda "state" $ lambda "namePair" $ lets [
      "currentMap">: first $ var "state",
      "currentSet">: second $ var "state",
      "name">: first $ var "namePair",
      "alias">: second $ var "namePair",
      "aliasStr">: unwrap H._ModuleName @@ var "alias"] $
      Logic.ifElse (Sets.member (var "alias") (var "currentSet"))
        (var "addPair" @@ var "state" @@ pair (var "name") (wrap H._ModuleName $ Strings.cat2 (var "aliasStr") (string "_")))
        (pair (Maps.insert (var "name") (var "alias") (var "currentMap")) (Sets.insert (var "alias") (var "currentSet")))] $
    Flows.pure $ Module.namespaces (var "focusPair") (var "resultMap")

newtypeAccessorNameDef :: TElement (Name -> String)
newtypeAccessorNameDef = haskellUtilsDefinition "newtypeAccessorName" $
  lambda "name" $
    Strings.cat2 (string "un") (ref Qnames.localNameOfDef @@ var "name")

rawNameDef :: TElement (String -> H.Name)
rawNameDef = haskellUtilsDefinition "rawName" $
  lambda "n" $
    inject H._Name H._Name_normal $
      record H._QualifiedName [
        H._QualifiedName_qualifiers>>: list [],
        H._QualifiedName_unqualified>>: wrap H._NamePart $ var "n"]

recordFieldReferenceDef :: TElement (HaskellNamespaces -> Name -> Name -> H.Name)
recordFieldReferenceDef = haskellUtilsDefinition "recordFieldReference" $
  lambda "namespaces" $ lambda "sname" $ lambda "fname" $ lets [
    "fnameStr">: unwrap _Name @@ var "fname",
    "qname">: ref Qnames.qualifyNameDef @@ var "sname",
    "ns">: Module.qualifiedNameNamespace $ var "qname",
    "typeNameStr">: ref typeNameForRecordDef @@ var "sname",
    "decapitalized">: ref Formatting.decapitalizeDef @@ var "typeNameStr",
    "capitalized">: ref Formatting.capitalizeDef @@ var "fnameStr",
    "nm">: Strings.cat2 (var "decapitalized") (var "capitalized"),
    "qualName">: record _QualifiedName [
      _QualifiedName_namespace>>: var "ns",
      _QualifiedName_local>>: var "nm"],
    "unqualName">: ref Qnames.unqualifyNameDef @@ var "qualName"] $
    ref elementReferenceDef @@ var "namespaces" @@ var "unqualName"

sanitizeHaskellNameDef :: TElement (String -> String)
sanitizeHaskellNameDef = haskellUtilsDefinition "sanitizeHaskellName" $
  ref Formatting.sanitizeWithUnderscoresDef @@ (ref HaskellLanguage.reservedWordsDef)

simpleNameDef :: TElement (String -> H.Name)
simpleNameDef = haskellUtilsDefinition "simpleName" $
  compose (ref rawNameDef) (ref sanitizeHaskellNameDef)

simpleValueBindingDef :: TElement (H.Name -> H.Expression -> Maybe H.LocalBindings -> H.ValueBinding)
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

toTypeApplicationDef :: TElement ([H.Type] -> H.Type)
toTypeApplicationDef = haskellUtilsDefinition "toTypeApplication" $
  lambda "types" $ lets [
    "app">: lambda "l" $
      Logic.ifElse (Equality.gt (Lists.length (var "l")) (int32 1))
        (inject H._Type H._Type_application $ record H._ApplicationType [
          H._ApplicationType_context>>: var "app" @@ (Lists.tail (var "l")),
          H._ApplicationType_argument>>: Lists.head (var "l")])
        (Lists.head $ var "l")] $
    var "app" @@ (Lists.reverse $ var "types")

typeNameForRecordDef :: TElement (Name -> String)
typeNameForRecordDef = haskellUtilsDefinition "typeNameForRecord" $
  lambda "sname" $ lets [
    "snameStr">: Core.unName $ var "sname",
    "parts">: Strings.splitOn (string ".") (var "snameStr")] $
    Lists.last $ var "parts"

unionFieldReferenceDef :: TElement (HaskellNamespaces -> Name -> Name -> H.Name)
unionFieldReferenceDef = haskellUtilsDefinition "unionFieldReference" $
  lambda "namespaces" $ lambda "sname" $ lambda "fname" $ lets [
    "fnameStr">: unwrap _Name @@ var "fname",
    "qname">: ref Qnames.qualifyNameDef @@ var "sname",
    "ns">: Module.qualifiedNameNamespace $ var "qname",
    "typeNameStr">: ref typeNameForRecordDef @@ var "sname",
    "capitalizedTypeName">: ref Formatting.capitalizeDef @@ var "typeNameStr",
    "capitalizedFieldName">: ref Formatting.capitalizeDef @@ var "fnameStr",
    "nm">: Strings.cat2 (var "capitalizedTypeName") (var "capitalizedFieldName"),
    "qualName">: record _QualifiedName [
      _QualifiedName_namespace>>: var "ns",
      _QualifiedName_local>>: var "nm"],
    "unqualName">: ref Qnames.unqualifyNameDef @@ var "qualName"] $
    ref elementReferenceDef @@ var "namespaces" @@ var "unqualName"

unpackForallTypeDef :: TElement (Graph -> Type -> ([Name], Type))
unpackForallTypeDef = haskellUtilsDefinition "unpackForallType" $
  lambdas ["cx", "t"] $ cases _Type (ref Strip.stripTypeDef @@ var "t")
    (Just $ pair (list []) (var "t")) [
    _Type_forall>>: lambda "fat" $ lets [
      "v">: Core.forallTypeParameter $ var "fat",
      "tbody">: Core.forallTypeBody $ var "fat",
      "recursiveResult">: ref unpackForallTypeDef @@ var "cx" @@ var "tbody",
      "vars">: first $ var "recursiveResult",
      "finalType">: second $ var "recursiveResult"] $
      pair (Lists.cons (var "v") (var "vars")) (var "finalType")]
