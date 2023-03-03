{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Testing where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types


hydraTestingModule :: Module Kv
hydraTestingModule = Module ns elements [hydraCoreModule] $
    Just "A model for unit testing"
  where
    ns = Namespace "hydra/testing"
    def = datatype ns
    core = nsref $ moduleNamespace hydraCoreModule
    testing = nsref ns

    elements = [

      def "TestCase" $
        doc "A simple test case with an input and an expected output" $
        lambda "a" $ record [
          "description">: optional string,
          "input">: core "Term" @@ "a",
          "output">: core "Term" @@ "a"],
          
      def "TestGroup" $
        doc "A collection of test cases with a name and optional description" $
        lambda "a" $ record [
          "name">: string,
          "description">: optional string,
          "subgroups">: list (testing "TestGroup" @@ "a"),
          "cases">: list (testing "TestCase" @@ "a")]]
