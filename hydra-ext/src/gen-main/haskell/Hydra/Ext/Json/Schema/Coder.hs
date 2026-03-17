-- Note: this is an automatically generated file. Do not edit.

-- | JSON Schema code generator: converts Hydra modules to JSON Schema documents

module Hydra.Ext.Json.Schema.Coder where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Json.Schema as Schema
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a Hydra module to a map of JSON Schema documents
moduleToJsonSchema :: t0 -> t1 -> t2 -> t3 -> t4 -> t5
moduleToJsonSchema opts mod defs cx g = moduleToJsonSchema opts mod defs cx g

-- | Construct JSON Schema documents from type definitions
constructModule :: t0 -> t1 -> t2 -> t3 -> t4 -> t5
constructModule cx g opts mod typeDefs = constructModule cx g opts mod typeDefs

-- | Encode a field type as a JSON Schema keyword-schema pair
encodeField :: t0 -> t1 -> Core.FieldType -> Either t2 (Schema.Keyword, Schema.Schema)
encodeField cx g ft =

      let name = Core.fieldTypeName ft
          typ = Core.fieldTypeType ft
      in (Eithers.map (\res -> (Schema.Keyword (Core.unName name), (Schema.Schema res))) (encodeType cx g False typ))

-- | Encode a Hydra name as a safe identifier string, replacing non-alphanumeric characters with underscores
encodeName :: Core.Name -> String
encodeName name = Formatting.nonAlnumToUnderscores (Core.unName name)

-- | Encode a named type as a list of JSON Schema restrictions with a title
encodeNamedType :: t0 -> t1 -> Core.Name -> Core.Type -> Either t2 [Schema.Restriction]
encodeNamedType cx g name typ =
    Eithers.map (\res -> Lists.concat [
      [
        Schema.RestrictionTitle (Core.unName name)],
      res]) (encodeType cx g False (Rewriting.deannotateType typ))

-- | Encode a Hydra type as a list of JSON Schema restrictions
encodeType :: t0 -> t1 -> t2 -> t3 -> t4
encodeType cx g optional typ = encodeType cx g optional typ

-- | Determine whether a field is required (i.e., not optional/Maybe)
isRequiredField :: Core.FieldType -> Bool
isRequiredField ft =

      let typ = Core.fieldTypeType ft
      in case (Rewriting.deannotateType typ) of
        Core.TypeMaybe _ -> False
        _ -> True

-- | Create a JSON Schema reference restriction for a named type
referenceRestriction :: Core.Name -> Schema.Restriction
referenceRestriction name =
    Schema.RestrictionReference (Schema.SchemaReference (Strings.cat [
      "#/$defs/",
      (encodeName name)]))
