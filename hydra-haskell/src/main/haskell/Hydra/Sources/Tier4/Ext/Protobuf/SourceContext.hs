{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Ext.Protobuf.SourceContext where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap


pbSourceContextNs = Namespace "hydra/ext/protobuf/sourceContext"
pbSourceContext = typeref pbSourceContextNs

protobufSourceContextModule :: Module
protobufSourceContextModule = Module pbSourceContextNs elements [hydraCoreModule] tier0Modules $
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
