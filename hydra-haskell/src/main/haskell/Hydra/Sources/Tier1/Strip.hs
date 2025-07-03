module Hydra.Sources.Tier1.Strip where

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


stripDefinition :: String -> TTerm a -> TElement a
stripDefinition = definitionInModule hydraStripModule

hydraStripModule :: Module
hydraStripModule = Module (Namespace "hydra.strip") elements [] [hydraCoreModule] $
    Just "Several functions for stripping annotations from types and terms."
  where
   elements = [
     el fullyStripTermDef,
     el stripTermDef,
     el stripTypeDef,
     el stripTypeParametersDef]

fullyStripTermDef :: TElement (Term -> Term)
fullyStripTermDef = stripDefinition "fullyStripTerm" $
  doc "Strip all annotations from a term, including first-class type annotations" $
  lambda "t" $ match _Term (Just $ var "t") [
    _Term_annotated>>: ref fullyStripTermDef <.> (project _AnnotatedTerm _AnnotatedTerm_subject)]
    @@ (var "t")

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
