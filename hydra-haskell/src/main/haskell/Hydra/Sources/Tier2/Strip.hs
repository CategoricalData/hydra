module Hydra.Sources.Tier2.Strip where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Sources.Tier1.All as Tier1
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


stripDefinition :: String -> TTerm a -> TElement a
stripDefinition = definitionInModule hydraStripModule

hydraStripModule :: Module
hydraStripModule = Module (Namespace "hydra.strip") elements [] [Tier1.hydraCoreModule] $
    Just "Several functions for stripping annotations from types and terms."
  where
   elements = [
     el stripTermDef,
     el stripTypeDef,
     el stripTypeParametersDef]

stripTermDef :: TElement (Term -> Term)
stripTermDef = stripDefinition "stripTerm" $
  doc "Strip all annotations from a term" $
  lambda "t" $ match _Term (Just $ var "t") [
    _Term_annotated>>: ref stripTermDef <.> (project _AnnotatedTerm _AnnotatedTerm_subject)]
    @@ (var "t")

stripTypeDef :: TElement (Type -> Type)
stripTypeDef = stripDefinition "stripType" $
  doc "Strip all annotations from a term" $
  lambda "t" $ match _Type (Just $ var "t") [
    _Type_annotated>>: ref stripTypeDef <.> (project _AnnotatedType _AnnotatedType_subject)]
    @@ (var "t")

stripTypeParametersDef :: TElement (Type -> Type)
stripTypeParametersDef = stripDefinition "stripTypeParameters" $
  doc "Strip any top-level type lambdas from a type, extracting the (possibly nested) type body" $
  lambda "t" $ cases _Type (ref stripTypeDef @@ var "t")
    (Just $ var "t") [
    _Type_forall>>: lambda "lt" (ref stripTypeParametersDef @@ (project _ForallType _ForallType_body @@ var "lt"))]
