{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Qnames where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors              as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Coders                 as Coders
import qualified Hydra.Dsl.Compute                as Compute
import qualified Hydra.Dsl.Core                   as Core
import qualified Hydra.Dsl.Graph                  as Graph
import qualified Hydra.Dsl.Lib.Chars              as Chars
import qualified Hydra.Dsl.Lib.Equality           as Equality
import qualified Hydra.Dsl.Lib.Flows              as Flows
import qualified Hydra.Dsl.Lib.Lists              as Lists
import qualified Hydra.Dsl.Lib.Literals           as Literals
import qualified Hydra.Dsl.Lib.Logic              as Logic
import qualified Hydra.Dsl.Lib.Maps               as Maps
import qualified Hydra.Dsl.Lib.Math               as Math
import qualified Hydra.Dsl.Lib.Optionals          as Optionals
import           Hydra.Dsl.Phantoms               as Phantoms
import qualified Hydra.Dsl.Lib.Sets               as Sets
import           Hydra.Dsl.Lib.Strings            as Strings
import qualified Hydra.Dsl.Mantle                 as Mantle
import qualified Hydra.Dsl.Module                 as Module
import qualified Hydra.Dsl.TTerms                 as TTerms
import qualified Hydra.Dsl.TTypes                 as TTypes
import qualified Hydra.Dsl.Terms                  as Terms
import qualified Hydra.Dsl.Topology               as Topology
import qualified Hydra.Dsl.Types                  as Types
import qualified Hydra.Dsl.Typing                 as Typing
import qualified Hydra.Sources.Tier1.All          as Tier1
import qualified Hydra.Sources.Tier1.Constants    as Constants
import qualified Hydra.Sources.Tier1.Encode.Core as EncodeCore
import qualified Hydra.Sources.Tier1.Decode       as Decode
import qualified Hydra.Sources.Tier1.Formatting   as Formatting
import qualified Hydra.Sources.Tier1.Literals     as Literals
import qualified Hydra.Sources.Tier1.Strip        as Strip
import           Prelude hiding ((++))
import qualified Data.Int                  as I
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y

-- Uncomment tier-2 sources as needed
--import qualified Hydra.Sources.Tier2.Adapt.Utils as AdaptUtils
--import qualified Hydra.Sources.Tier2.Adapt.Modules as AdaptModules
--import qualified Hydra.Sources.Tier2.Annotations as Annotations
--import qualified Hydra.Sources.Tier2.Arity as Arity
--import qualified Hydra.Sources.Tier2.Decode.Core as DecodeCore
--import qualified Hydra.Sources.Tier2.Languages as Languages
--import qualified Hydra.Sources.Tier2.Errors as Errors
--import qualified Hydra.Sources.Tier2.Extract.Core as ExtractCore
--import qualified Hydra.Sources.Tier2.Monads as Monads
--import qualified Hydra.Sources.Tier2.Grammars as Grammars
--import qualified Hydra.Sources.Tier2.Inference as Inference
--import qualified Hydra.Sources.Tier2.Lexical as Lexical
--import qualified Hydra.Sources.Tier2.Adapt.Literals as AdaptLiterals
--import qualified Hydra.Sources.Tier2.Describe.Core as DescribeCore
--import qualified Hydra.Sources.Tier2.Qnames as Qnames
--import qualified Hydra.Sources.Tier2.Reduction as Reduction
--import qualified Hydra.Sources.Tier2.Rewriting as Rewriting
--import qualified Hydra.Sources.Tier2.Schemas as Schemas
--import qualified Hydra.Sources.Tier2.Serialization as Serialization
--import qualified Hydra.Sources.Tier2.Show.Accessors as ShowAccessors
--import qualified Hydra.Sources.Tier2.Show.Core as ShowCore
--import qualified Hydra.Sources.Tier2.Sorting as Sorting
--import qualified Hydra.Sources.Tier2.Substitution as Substitution
--import qualified Hydra.Sources.Tier2.Tarjan as Tarjan
--import qualified Hydra.Sources.Tier2.Templating as Templating
--import qualified Hydra.Sources.Tier2.Adapt.Terms as AdaptTerms
--import qualified Hydra.Sources.Tier2.Unification as Unification
--import qualified Hydra.Sources.Tier2.Variants as Variants


qnamesDefinition :: String -> TTerm a -> TElement a
qnamesDefinition = definitionInModule hydraQnamesModule

hydraQnamesModule :: Module
hydraQnamesModule = Module (Namespace "hydra.qnames") elements
    [Formatting.hydraFormattingModule]
    [Tier1.hydraMantleModule, Tier1.hydraModuleModule] $
    Just ("Functions for working with qualified names.")
  where
   elements = [
     el localNameOfDef,
     el namespaceOfDef,
     el namespaceToFilePathDef,
     el qnameDef,
     el qualifyNameDef,
     el unqualifyNameDef]

localNameOfDef :: TElement (Name -> String)
localNameOfDef = qnamesDefinition "localNameOf" $
  unaryFunction Module.qualifiedNameLocal <.> ref qualifyNameDef

namespaceOfDef :: TElement (Name -> Maybe Namespace)
namespaceOfDef = qnamesDefinition "namespaceOf" $
  unaryFunction Module.qualifiedNameNamespace <.> ref qualifyNameDef

namespaceToFilePathDef :: TElement (CaseConvention -> FileExtension -> Namespace -> String)
namespaceToFilePathDef = qnamesDefinition "namespaceToFilePath" $
  lambda "caseConv" $ lambda "ext" $ lambda "ns" $ lets [
    "parts">: Lists.map
      (ref Formatting.convertCaseDef @@ Mantle.caseConventionCamel @@ var "caseConv")
      (Strings.splitOn "." (Core.unNamespace $ var "ns"))]
    $ (Strings.intercalate "/" $ var "parts") ++ "." ++ (Module.unFileExtension $ var "ext")

qnameDef :: TElement (Namespace -> String -> Name)
qnameDef = qnamesDefinition "qname" $
  doc "Construct a qualified (dot-separated) name" $
  lambda "ns" $ lambda "name" $
    wrap _Name $
      Strings.cat $
        list [apply (unwrap _Namespace) (var "ns"), string ".", var "name"]

qualifyNameDef :: TElement (Name -> QualifiedName)
qualifyNameDef = qnamesDefinition "qualifyName" $
  lambda "name" $ lets [
    "parts">: Lists.reverse (Strings.splitOn "." (Core.unName $ var "name"))]
    $ Logic.ifElse
      (Equality.equal (int32 1) (Lists.length $ var "parts"))
      (Module.qualifiedName nothing (Core.unName $ var "name"))
      (Module.qualifiedName
        (just $ wrap _Namespace (Strings.intercalate "." (Lists.reverse (Lists.tail $ var "parts"))))
        (Lists.head $ var "parts"))

unqualifyNameDef :: TElement (QualifiedName -> Name)
unqualifyNameDef = qnamesDefinition "unqualifyName" $
  doc "Convert a qualified name to a dot-separated name" $
  lambda "qname" $ lets [
    "prefix">: Optionals.maybe
      (string "")
      (lambda "n" $ (unwrap _Namespace @@ var "n") ++ string ".")
      (project _QualifiedName _QualifiedName_namespace @@ var "qname")]
    $ wrap _Name $ var "prefix" ++ (project _QualifiedName _QualifiedName_local @@ var "qname")
