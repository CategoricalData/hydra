{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Tinkerpop.Errors where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types

import Hydra.Sources.Tier4.Langs.Tinkerpop.PropertyGraph


tinkerpopErrorsModule :: Module Kv
tinkerpopErrorsModule = Module ns elements [tinkerpopPropertyGraphModule] $
    Just ("A model for property graph validation errors")
  where
    ns = Namespace "hydra/langs/tinkerpop/errors"
    pg = typeref $ moduleNamespace tinkerpopPropertyGraphModule
    errors = typeref ns
    def = datatype ns

    elements = [

      def "BadEdge" $
        lambda "t" $ lambda "v" $
          union [
            "label">:
              doc "An edge label mismatch" $
              errors "EdgeLabelMismatch",
            "labelUnexpected">:
              doc "The label of the edge does not have an associated edge type" $
              pg "EdgeLabel",
            "id">:
              doc "An edge id type error" $
              errors "TypeError" @@ "t" @@ "v",
            "property">:
              doc "An edge property error" $
              errors "BadProperty" @@ "t" @@ "v",
            "noSuchOutVertex">:
              doc "The out-vertex of the edge does not exist"
              "v",
            "noSuchInVertex">:
              doc "The in-vertex of the edge does not exist"
              "v",
            "wrongOutVertexLabel">:
              doc "The out-vertex of the edge has the wrong label" $
              errors "VertexLabelMismatch",
            "wrongInVertexLabel">:
              doc "The in-vertex of the edge has the wrong label" $
              errors "VertexLabelMismatch"],

      def "BadProperty" $
        lambda "t" $ lambda "v" $
          union [
            "unexpectedKey">:
              doc "The property key does not have an associated type" $
              pg "PropertyKey",
            "missingKey">:
              doc "A required property is missing" $
              pg "PropertyKey",
            "value">:
              doc "A property value is invalid" $
              errors "TypeError" @@ "t" @@ "v"],

      def "BadVertex" $
        lambda "t" $ lambda "v" $
          union [
            "label">:
              doc "A vertex label mismatch" $
              errors "VertexLabelMismatch",
            "labelUnexpected">:
              doc "The label of the vertex does not have an associated vertex type" $
              pg "VertexLabel",
            "id">:
              doc "A vertex id type error" $
              errors "TypeError" @@ "t" @@ "v",
            "property">:
              doc "A vertex property error" $
              errors "BadProperty" @@ "t" @@ "v"],

      def "EdgeLabelMismatch" $
        record [
          "expected">:
            doc "The expected edge label, based on the edge type" $
            pg "EdgeLabel",
          "actual">:
            doc "The actual edge label" $
            pg "EdgeLabel"],

      def "EdgeValidationError" $
        lambda "t" $ lambda "v" $
          record [
            "id">:
              doc "The id of the edge which failed validation"
              "v",
            "error">:
              doc "A specific validation error for the edge" $
              errors "BadEdge" @@ "t" @@ "v"],

      def "ElementValidationError" $
        lambda "t" $ lambda "v" $
          union [
            "vertex">:
              doc "A vertex validation error" $
              errors "VertexValidationError" @@ "t" @@ "v",
            "edge">:
              doc "An edge validation error" $
              errors "EdgeValidationError" @@ "t" @@ "v"],

      def "TypeError" $
        lambda "t" $ lambda "v" $
          record [
            "expectedType">:
              doc "An expected type"
              "t",
            "actualValue">:
              doc "The actual value, which does not conform to the expected type"
              "v"],

      def "VertexLabelMismatch" $
        record [
          "expected">:
            doc "The expected vertex label, based on the vertex type" $
            pg "VertexLabel",
          "actual">:
            doc "The actual vertex label" $
            pg "VertexLabel"],

      def "VertexValidationError" $
        lambda "t" $ lambda "v" $
          record [
            "id">:
              doc "The id of the vertex which failed validation"
              "v",
            "error">:
              doc "A specific validation error for the vertex" $
              errors "BadVertex" @@ "t" @@ "v"]]
