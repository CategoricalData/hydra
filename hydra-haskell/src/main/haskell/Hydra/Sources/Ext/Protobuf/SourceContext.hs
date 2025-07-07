{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Ext.Protobuf.SourceContext where

import Hydra.Kernel
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.All as Tier2
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap


pbSourceContextNs = Namespace "hydra.ext.protobuf.sourceContext"
pbSourceContext = typeref pbSourceContextNs

protobufSourceContextModule :: Module
protobufSourceContextModule = Module pbSourceContextNs elements [KernelTypes.hydraCoreModule] [KernelTypes.hydraCoreModule] $
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
