{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Ext.Protobuf.SourceContext where

import Hydra.Kernel
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Core


pbSourceContextNs = Namespace "hydra/ext/protobuf/sourceContext"
pbSourceContext = nsref pbSourceContextNs

protobufSourceContextModule :: Module Meta
protobufSourceContextModule = Module pbSourceContextNs elements [] $
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
