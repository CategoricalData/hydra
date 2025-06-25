module Hydra.Sources.Tier1.Constants where

-- Standard term-level Tier-1 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Tier0.Core
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


constantsDefinition :: String -> TTerm a -> TElement a
constantsDefinition = definitionInModule hydraConstantsModule

hydraConstantsModule :: Module
hydraConstantsModule = Module (Namespace "hydra.constants") elements [] [hydraCoreModule] $
    Just ("A module for tier-0 constants.")
  where
   elements = [
     el ignoredVariableDef,
     el key_classesDef,
     el key_debugIdDef,
     el key_deprecatedDef,
     el key_descriptionDef,
     el key_excludeDef,
     el key_firstClassTypeDef,
     el key_maxLengthDef,
     el key_minLengthDef,
     el key_preserveFieldNameDef,
     el key_typeDef,
     el maxInt32Def,
     el placeholderNameDef,
     el maxTraceDepthDef]

annotationKeyDef :: String -> Maybe String -> TElement Name
annotationKeyDef name mdesc = constantsDefinition ("key_" <> name) $ case mdesc of
    Nothing -> def
    Just comment -> doc comment def
  where
    def = wrap _Name $ string name

ignoredVariableDef :: TElement String
ignoredVariableDef = constantsDefinition "ignoredVariable" $
  string "_"

key_classesDef = annotationKeyDef "classes" Nothing
key_debugIdDef = annotationKeyDef "debugId" Nothing
key_deprecatedDef = annotationKeyDef "_deprecated" Nothing
key_descriptionDef = annotationKeyDef "description" Nothing
key_excludeDef = annotationKeyDef "exclude" Nothing
key_firstClassTypeDef = annotationKeyDef "firstClassType"
  $ Just "A flag which tells the language coders to encode a given encoded type as a term rather than a native type"
key_maxLengthDef = annotationKeyDef "_maxLength" Nothing
key_minLengthDef = annotationKeyDef "_minLength" Nothing
key_preserveFieldNameDef = annotationKeyDef "_preserveFieldName" Nothing
key_typeDef = annotationKeyDef "type" Nothing

maxInt32Def :: TElement Int
maxInt32Def = constantsDefinition "maxInt32" $
  doc "The maximum value of a 32-bit integer" $
  int32 maxBound

placeholderNameDef :: TElement Name
placeholderNameDef = constantsDefinition "placeholderName" $
  doc "A placeholder name for row types as they are being constructed" $
  wrap _Name $ string "Placeholder"

maxTraceDepthDef :: TElement Int
maxTraceDepthDef = constantsDefinition "maxTraceDepth" $
  doc ("A maximum depth for nested flows."
    <> " Currently, this is set very high because deep flows are common in type inference over the Hydra kernel.") $
  int32 4000
