-- | Imperative coder: converts Hydra modules to imperative modules.

module Hydra.Sources.Imperative.Coder where

import Hydra.Kernel
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import           Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Hydra.Sources.Kernel.Types.Imperative as ImperativeTypes
import qualified Hydra.Imperative as Imp


def :: String -> TTerm a -> TBinding a
def = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.ext.imperative.coder"

module_ :: Module
module_ = Module ns elements
    [Rewriting.ns, Schemas.ns]
    (ImperativeTypes.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Imperative coder: converts Hydra modules to imperative modules."
  where
    elements = [
      toBinding moduleToImperative,
      toBinding termDefinitionToMember,
      toBinding termToStatement,
      toBinding termToExpression,
      toBinding lambdaToProcedure]


moduleToImperative :: TBinding (
  Language -> Module -> [Definition] -> Context -> Graph
  -> Either (InContext Error) Imp.ImperativeModule)
moduleToImperative = def "moduleToImperative" $
  doc "Convert a Hydra module to an imperative module" $
  lambda "lang" $ lambda "mod" $ lambda "defs" $ lambda "cx" $ lambda "g" $
    "partitioned" <~ Schemas.partitionDefinitions @@ var "defs" $
    "typeDefs" <~ Pairs.first (var "partitioned") $
    "termDefs" <~ Pairs.second (var "partitioned") $
    Eithers.bind
      (Eithers.mapList
        ("td" ~> asTerm termDefinitionToMember @@ var "cx" @@ var "g" @@ var "td")
        (var "termDefs"))
      ("members" ~>
        right (record Imp._ImperativeModule [
          Imp._ImperativeModule_namespace>>: Module.moduleNamespace (var "mod"),
          Imp._ImperativeModule_typeDefinitions>>: var "typeDefs",
          Imp._ImperativeModule_memberDefinitions>>: var "members",
          Imp._ImperativeModule_dependencies>>: Module.moduleTermDependencies (var "mod"),
          Imp._ImperativeModule_description>>: Module.moduleDescription (var "mod")]))


termDefinitionToMember :: TBinding (
  Context -> Graph -> TermDefinition
  -> Either (InContext Error) Imp.MemberDefinition)
termDefinitionToMember = def "termDefinitionToMember" $
  doc "Convert a term definition to a member definition (procedure or constant)" $
  lambda "cx" $ lambda "g" $ lambda "td" $
    "name" <~ project _TermDefinition _TermDefinition_name @@ var "td" $
    "term" <~ project _TermDefinition _TermDefinition_term @@ var "td" $
    "tscheme" <~ project _TermDefinition _TermDefinition_type @@ var "td" $
    "deannotated" <~ Rewriting.deannotateTerm @@ var "term" $
    cases _Term (var "deannotated")
      (Just $
        right (inject Imp._MemberDefinition Imp._MemberDefinition_constant (var "td")))
      [_Term_function>>: ("fn" ~>
        cases _Function (var "fn")
          (Just $
            right (inject Imp._MemberDefinition Imp._MemberDefinition_constant (var "td")))
          [_Function_lambda>>: ("lam" ~>
            Eithers.bind
              (asTerm lambdaToProcedure @@ var "cx" @@ var "g" @@ var "name" @@ var "lam" @@ var "tscheme")
              ("proc" ~>
                right (inject Imp._MemberDefinition Imp._MemberDefinition_procedure
                  (record Imp._ProcedureDefinition [
                    Imp._ProcedureDefinition_name>>: var "name",
                    Imp._ProcedureDefinition_procedure>>: var "proc",
                    Imp._ProcedureDefinition_typeScheme>>: var "tscheme"]))))])]


-- | Convert a lambda to a procedure. For simplicity, only peels the outermost lambda.
-- Nested lambdas in the body are left as pure expressions.
lambdaToProcedure :: TBinding (
  Context -> Graph -> Name -> Lambda -> TypeScheme
  -> Either (InContext Error) Imp.Procedure)
lambdaToProcedure = def "lambdaToProcedure" $
  doc "Convert a lambda term into a procedure" $
  lambda "cx" $ lambda "g" $ lambda "name" $ lambda "lam" $ lambda "tscheme" $
    "param" <~ record Imp._Parameter [
      Imp._Parameter_name>>: project _Lambda _Lambda_parameter @@ var "lam",
      Imp._Parameter_type>>: Maybes.fromMaybe
        (inject _Type _Type_variable (wrap _Name (string "a")))
        (project _Lambda _Lambda_domain @@ var "lam")] $
    "bodyTerm" <~ project _Lambda _Lambda_body @@ var "lam" $
    -- Check if the body is another lambda; if so, peel it too
    cases _Term (Rewriting.deannotateTerm @@ var "bodyTerm")
      (Just $
        -- Body is not a lambda: convert it to a statement
        Eithers.bind
          (asTerm termToStatement @@ var "cx" @@ var "g" @@ var "bodyTerm")
          ("bodyStmt" ~>
            right (record Imp._Procedure [
              Imp._Procedure_name>>: var "name",
              Imp._Procedure_parameters>>: list [var "param"],
              Imp._Procedure_returnType>>: nothing,
              Imp._Procedure_body>>: var "bodyStmt"])))
      [_Term_function>>: ("fn" ~>
        cases _Function (var "fn")
          (Just $
            Eithers.bind
              (asTerm termToStatement @@ var "cx" @@ var "g" @@ var "bodyTerm")
              ("bodyStmt" ~>
                right (record Imp._Procedure [
                  Imp._Procedure_name>>: var "name",
                  Imp._Procedure_parameters>>: list [var "param"],
                  Imp._Procedure_returnType>>: nothing,
                  Imp._Procedure_body>>: var "bodyStmt"])))
          [_Function_lambda>>: ("innerLam" ~>
            -- Recursively peel the inner lambda
            Eithers.bind
              (asTerm lambdaToProcedure @@ var "cx" @@ var "g" @@ var "name" @@ var "innerLam" @@ var "tscheme")
              ("innerProc" ~>
                right (record Imp._Procedure [
                  Imp._Procedure_name>>: var "name",
                  Imp._Procedure_parameters>>:
                    Lists.cons (var "param") (project Imp._Procedure Imp._Procedure_parameters @@ var "innerProc"),
                  Imp._Procedure_returnType>>: project Imp._Procedure Imp._Procedure_returnType @@ var "innerProc",
                  Imp._Procedure_body>>: project Imp._Procedure Imp._Procedure_body @@ var "innerProc"])))])]


termToStatement :: TBinding (
  Context -> Graph -> Term
  -> Either (InContext Error) Imp.Statement)
termToStatement = def "termToStatement" $
  doc "Convert a term to an imperative statement" $
  lambda "cx" $ lambda "g" $ lambda "term" $
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just $
        right (inject Imp._Statement Imp._Statement_return
          (just (inject Imp._Expression Imp._Expression_pure (var "term")))))
      [_Term_let>>: ("lt" ~>
        "bindings" <~ project _Let _Let_bindings @@ var "lt" $
        "body" <~ project _Let _Let_body @@ var "lt" $
        "declStmts" <~ Lists.map
          ("b" ~> inject Imp._Statement Imp._Statement_declare
            (record Imp._Declaration [
              Imp._Declaration_name>>: project _Binding _Binding_name @@ var "b",
              Imp._Declaration_type>>: Maybes.map
                ("ts" ~> project _TypeScheme _TypeScheme_type @@ var "ts")
                (project _Binding _Binding_type @@ var "b"),
              Imp._Declaration_value>>: just (inject Imp._Expression Imp._Expression_pure
                (project _Binding _Binding_term @@ var "b")),
              Imp._Declaration_mutable>>: boolean False]))
          (var "bindings") $
        Eithers.bind
          (asTerm termToStatement @@ var "cx" @@ var "g" @@ var "body")
          ("bodyStmt" ~>
            right (inject Imp._Statement Imp._Statement_block
              (Lists.concat2 (var "declStmts") (list [var "bodyStmt"])))))]


termToExpression :: TBinding (
  Context -> Graph -> Term
  -> Either (InContext Error) Imp.Expression)
termToExpression = def "termToExpression" $
  doc "Convert a term to an imperative expression" $
  lambda "cx" $ lambda "g" $ lambda "term" $
    cases _Term (Rewriting.deannotateTerm @@ var "term")
      (Just $
        right (inject Imp._Expression Imp._Expression_pure (var "term")))
      [_Term_let>>: ("lt" ~>
        "bindings" <~ project _Let _Let_bindings @@ var "lt" $
        "body" <~ project _Let _Let_body @@ var "lt" $
        "declStmts" <~ Lists.map
          ("b" ~> inject Imp._Statement Imp._Statement_declare
            (record Imp._Declaration [
              Imp._Declaration_name>>: project _Binding _Binding_name @@ var "b",
              Imp._Declaration_type>>: Maybes.map
                ("ts" ~> project _TypeScheme _TypeScheme_type @@ var "ts")
                (project _Binding _Binding_type @@ var "b"),
              Imp._Declaration_value>>: just (inject Imp._Expression Imp._Expression_pure
                (project _Binding _Binding_term @@ var "b")),
              Imp._Declaration_mutable>>: boolean False]))
          (var "bindings") $
        right (inject Imp._Expression Imp._Expression_do
          (record Imp._DoExpression [
            Imp._DoExpression_statements>>: var "declStmts",
            Imp._DoExpression_result>>: inject Imp._Expression Imp._Expression_pure (var "body")])))]
