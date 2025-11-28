{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Parsing where

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


module_ :: Module
module_ = Module ns elements [] [Core.module_] $
    Just "Parser combinator types for text parsing"
  where
    ns = Namespace "hydra.parsing"
    core = typeref $ moduleNamespace Core.module_
    parsing = typeref ns

    def = datatype ns

    elements = [

      def "ParseError" $
        doc "An error which occurred while parsing" $
        record [
          "message">:
            doc "An error message" $
            string,
          "remainder">:
            doc "The remaining input at the point of failure" $
            string],

      def "ParseResult" $
        doc "The result of a parse operation" $
        forAll "a" $ union [
          "success">:
            doc "A successful parse, with a value and the remaining unparsed input" $
            parsing "ParseSuccess" @@ "a",
          "failure">:
            doc "A failed parse, with an error message and the remaining input" $
            parsing "ParseError"],

      def "ParseSuccess" $
        doc "A successful parse result" $
        forAll "a" $ record [
          "value">:
            doc "The parsed value" $
            "a",
          "remainder">:
            doc "The remaining unparsed input" $
            string],

      def "Parser" $
        doc "A parser which consumes characters from a string and produces a value" $
        forAll "a" $ wrap $
          string --> parsing "ParseResult" @@ "a"]
