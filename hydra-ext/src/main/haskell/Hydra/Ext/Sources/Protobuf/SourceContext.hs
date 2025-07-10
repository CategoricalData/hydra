{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Protobuf.SourceContext where

import Hydra.Kernel
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap

import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Mantle      as Mantle
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow


pbSourceContextNs = Namespace "hydra.ext.protobuf.sourceContext"
pbSourceContext = typeref pbSourceContextNs

protobufSourceContextModule :: Module
protobufSourceContextModule = Module pbSourceContextNs elements [Core.module_] [Core.module_] $
    Just "Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/source_context.proto"
  where
    def = datatype pbSourceContextNs

    elements = [
--  // `SourceContext` represents information about the source of a
--  // protobuf element, like the file in which it is defined.
--  message SourceContext {
      def "SourceContext" $
        doc ("`SourceContext` represents information about the source of a " ++
             "protobuf element, like the file in which it is defined.") $
        record [
--    // The path-qualified name of the .proto file that contained the associated
--    // protobuf element.  For example: `"google/protobuf/source_context.proto"`.
--    string file_name = 1;
          "fileName">:
            doc ("The path-qualified name of the .proto file that contained the associated " ++
                 "protobuf element.  For example: `\"google/protobuf/source_context.proto\"`.")
            string]]
--  }
