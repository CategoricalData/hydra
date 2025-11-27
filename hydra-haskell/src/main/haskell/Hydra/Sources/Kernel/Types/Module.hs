{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Module where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms                 as Terms
import           Hydra.Dsl.Types                 as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

import qualified Hydra.Sources.Kernel.Types.Graph as Graph


module_ :: Module
module_ = Module ns elements [Graph.module_] [Core.module_] $
    Just "A model for Hydra namespaces and modules"
  where
    ns = Namespace "hydra.module"
    core = typeref $ moduleNamespace Core.module_
    graph = typeref $ moduleNamespace Graph.module_
    mod = typeref ns
    def = datatype ns

    elements = [

      def "Definition" $
        doc "A definition, which may be either a term or type definition" $
        union [
          "term">:
            doc "A term definition" $
            mod "TermDefinition",
          "type">:
            doc "A type definition" $
            mod "TypeDefinition"],

      def "FileExtension" $
        doc "A file extension (without the dot), e.g. \"json\" or \"py\"" $
        wrap string,

      def "Library" $
        doc "A library of primitive functions" $
        record [
          "namespace">:
            doc "A common prefix for all primitive function names in the library" $
            mod "Namespace",
          "prefix">:
            doc "A preferred namespace prefix for function names in the library" $
            string,
          "primitives">:
            doc "The primitives defined in this library" $
            list $ graph "Primitive"],

      def "Module" $
        doc "A logical collection of elements in the same namespace, having dependencies on zero or more other modules" $
        record [
          "namespace">:
            doc "A common prefix for all element names in the module" $
            mod "Namespace",
          "elements">:
            doc "The elements defined in this module" $
            list $ core "Binding",
          "termDependencies">:
            doc "Any modules which the term expressions of this module directly depend upon" $
            list $ mod "Module",
          "typeDependencies">:
            doc "Any modules which the type expressions of this module directly depend upon" $
            list $ mod "Module",
          "description">:
            doc "An optional human-readable description of the module" $
            optional string],

      def "Namespace" $
        doc "A prefix for element names" $
        wrap string,

      def "Namespaces" $
        doc "A mapping from namespaces to values of type n, with a focus on one namespace" $
        forAll "n" $ record [
          "focus">:
            doc "The namespace in focus, together with its associated value" $
            pair (mod "Namespace") (var "n"),
          "mapping">:
            doc "A mapping of namespaces to values" $
            Types.map (mod "Namespace") (var "n")],

      def "QualifiedName" $
        doc "A qualified name consisting of an optional namespace together with a mandatory local name" $
        record [
          "namespace">:
            doc "The optional namespace" $
            optional $ mod "Namespace",
          "local">:
            doc "The local name" $
            string],

      def "TermDefinition" $
        doc "A term-level definition, including a name, a term, and the type of the term" $
        record [
          "name">:
            doc "The name of the term" $
            core "Name",
          "term">:
            doc "The term being defined" $
            core "Term",
          "type">:
            doc "The type of the term" $
            core "Type"],

     def "TypeDefinition" $
        doc "A type-level definition, including a name and the type" $
        record [
          "name">:
            doc "The name of the type" $
            core "Name",
          -- TODO: consider using TypeScheme here instead of Type
          "type">:
            doc "The type being defined" $
            core "Type"]]
