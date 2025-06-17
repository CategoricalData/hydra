-- | Abstractions for paired transformations between languages

module Hydra.Coders where

import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Mantle as Mantle
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An evaluation context together with a source language and a target language
data AdapterContext = 
  AdapterContext {
    adapterContextGraph :: Graph.Graph,
    adapterContextLanguage :: Language,
    adapterContextAdapters :: (M.Map Core.Name (Compute.Adapter AdapterContext AdapterContext Core.Type Core.Type Core.Term Core.Term))}

_AdapterContext = (Core.Name "hydra.coders.AdapterContext")

_AdapterContext_graph = (Core.Name "graph")

_AdapterContext_language = (Core.Name "language")

_AdapterContext_adapters = (Core.Name "adapters")

-- | Indicates either the 'out' or the 'in' direction of a coder
data CoderDirection = 
  CoderDirectionEncode  |
  CoderDirectionDecode 
  deriving (Eq, Ord, Read, Show)

_CoderDirection = (Core.Name "hydra.coders.CoderDirection")

_CoderDirection_encode = (Core.Name "encode")

_CoderDirection_decode = (Core.Name "decode")

-- | A named language together with language-specific constraints
data Language = 
  Language {
    languageName :: LanguageName,
    languageConstraints :: LanguageConstraints}

_Language = (Core.Name "hydra.coders.Language")

_Language_name = (Core.Name "name")

_Language_constraints = (Core.Name "constraints")

-- | A set of constraints on valid type and term expressions, characterizing a language
data LanguageConstraints = 
  LanguageConstraints {
    -- | All supported elimination variants
    languageConstraintsEliminationVariants :: (S.Set Mantle.EliminationVariant),
    -- | All supported literal variants
    languageConstraintsLiteralVariants :: (S.Set Mantle.LiteralVariant),
    -- | All supported float types
    languageConstraintsFloatTypes :: (S.Set Core.FloatType),
    -- | All supported function variants
    languageConstraintsFunctionVariants :: (S.Set Mantle.FunctionVariant),
    -- | All supported integer types
    languageConstraintsIntegerTypes :: (S.Set Core.IntegerType),
    -- | All supported term variants
    languageConstraintsTermVariants :: (S.Set Mantle.TermVariant),
    -- | All supported type variants
    languageConstraintsTypeVariants :: (S.Set Mantle.TypeVariant),
    -- | A logical set of types, as a predicate which tests a type for inclusion
    languageConstraintsTypes :: (Core.Type -> Bool)}

_LanguageConstraints = (Core.Name "hydra.coders.LanguageConstraints")

_LanguageConstraints_eliminationVariants = (Core.Name "eliminationVariants")

_LanguageConstraints_literalVariants = (Core.Name "literalVariants")

_LanguageConstraints_floatTypes = (Core.Name "floatTypes")

_LanguageConstraints_functionVariants = (Core.Name "functionVariants")

_LanguageConstraints_integerTypes = (Core.Name "integerTypes")

_LanguageConstraints_termVariants = (Core.Name "termVariants")

_LanguageConstraints_typeVariants = (Core.Name "typeVariants")

_LanguageConstraints_types = (Core.Name "types")

-- | The unique name of a language
newtype LanguageName = 
  LanguageName {
    unLanguageName :: String}
  deriving (Eq, Ord, Read, Show)

_LanguageName = (Core.Name "hydra.coders.LanguageName")

-- | A bidirectional encoder which maps between the same type and term languages on either side
type SymmetricAdapter s t v = (Compute.Adapter s s t t v v)

_SymmetricAdapter = (Core.Name "hydra.coders.SymmetricAdapter")

-- | Specifies either a pre-order or post-order traversal
data TraversalOrder = 
  -- | Pre-order traversal
  TraversalOrderPre  |
  -- | Post-order traversal
  TraversalOrderPost 
  deriving (Eq, Ord, Read, Show)

_TraversalOrder = (Core.Name "hydra.coders.TraversalOrder")

_TraversalOrder_pre = (Core.Name "pre")

_TraversalOrder_post = (Core.Name "post")

-- | A function which maps a Hydra type to a symmetric adapter between types and terms
type TypeAdapter = (Core.Type -> Compute.Flow AdapterContext (SymmetricAdapter AdapterContext Core.Type Core.Term))

_TypeAdapter = (Core.Name "hydra.coders.TypeAdapter")
