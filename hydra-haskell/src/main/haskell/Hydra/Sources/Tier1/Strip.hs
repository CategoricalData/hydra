module Hydra.Sources.Tier1.Strip where

-- Standard Tier-1 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier0.All


stripDefinition :: String -> Datum a -> Definition a
stripDefinition = definitionInModule hydraStripModule

hydraStripModule :: Module
hydraStripModule = Module (Namespace "hydra/strip") elements [] tier0Modules $
    Just "Several functions for stripping annotations from types and terms."
  where
   elements = [
     el fullyStripTermDef,
     el stripTermDef,
     el stripTypeDef,
     el stripTypeParametersDef]

fullyStripTermDef :: Definition (Term -> Term)
fullyStripTermDef = stripDefinition "fullyStripTerm" $
    doc "Strip all annotations from a term, including first-class type annotations" $
    function termT termT $
      lambda "t" (match _Term (Just $ var "t") [
        Case _Term_annotated --> ref fullyStripTermDef <.> (project _AnnotatedTerm _AnnotatedTerm_subject),
        Case _Term_typed --> ref fullyStripTermDef <.> (project _TypedTerm _TypedTerm_term)
        ] @@ (var "t"))

stripTermDef :: Definition (Term -> Term)
stripTermDef = stripDefinition "stripTerm" $
    doc "Strip all annotations from a term" $
    function termT termT $
      lambda "t" (match _Term (Just $ var "t") [
        Case _Term_annotated --> ref stripTermDef <.> (project _AnnotatedTerm _AnnotatedTerm_subject)
        ] @@ (var "t"))

stripTypeDef :: Definition (Type -> Type)
stripTypeDef = stripDefinition "stripType" $
    doc "Strip all annotations from a term" $
    function typeT typeT $
      lambda "t" (match _Type (Just $ var "t") [
        Case _Type_annotated --> ref stripTypeDef <.> (project _AnnotatedType _AnnotatedType_subject)
        ] @@ (var "t"))

stripTypeParametersDef :: Definition (Type -> Type)
stripTypeParametersDef = stripDefinition "stripTypeParameters" $
    doc "Strip any top-level type lambdas from a type, extracting the (possibly nested) type body" $
    function typeT typeT $
      lambda "t" $ match _Type (Just $ var "t") [
        Case _Type_lambda --> lambda "lt" (ref stripTypeParametersDef @@ (project _LambdaType _LambdaType_body @@ var "lt"))
        ] @@ (ref stripTypeDef @@ var "t")
