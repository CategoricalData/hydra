module Hydra.Sources.Tier2.Extras (hydraExtrasModule) where

-- Standard Tier-2 imports
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
import           Hydra.Sources.Tier1.All


hydraExtrasDefinition :: String -> TTerm a -> TElement a
hydraExtrasDefinition = definitionInModule hydraExtrasModule

hydraExtrasModule :: Module
hydraExtrasModule = Module (Namespace "hydra/extras") elements
    [hydraGraphModule, hydraMantleModule, hydraComputeModule]
    tier0Modules $
    Just "Basic functions which depend on primitive functions"
  where
    elements = [
      el functionArityDef,
      el lookupPrimitiveDef,
      el primitiveArityDef,
      el qnameDef,
      el termArityDef,
      el typeArityDef,
      el uncurryTypeDef,
      el getAnnotationDef
      ]

functionArityDef :: TElement (Function -> Int)
functionArityDef = hydraExtrasDefinition "functionArity" $
  function (TypeVariable _Function) Types.int32 $
  match _Function Nothing [
    TCase _Function_elimination --> constant (int32 1),
    TCase _Function_lambda --> (Math.add @@ int32 1) <.> (ref termArityDef <.> Core.lambdaBody),
    TCase _Function_primitive --> constant $
      doc "TODO: This function needs to be monadic, so we can look up the primitive" (int32 42)]

lookupPrimitiveDef :: TElement (Graph -> Name -> Maybe (Primitive))
lookupPrimitiveDef = hydraExtrasDefinition "lookupPrimitive" $
  function
    graphT
    (Types.function nameT (optionalT primitiveT)) $
  lambda "g" $ lambda "name" $
    apply (Maps.lookup @@ var "name") (Graph.graphPrimitives @@ var "g")

primitiveArityDef :: TElement (Primitive -> Int)
primitiveArityDef = hydraExtrasDefinition "primitiveArity" $
  doc "Find the arity (expected number of arguments) of a primitive constant or function" $
  function primitiveT Types.int32 $
  (ref typeArityDef <.> Core.typeSchemeType <.> Graph.primitiveType)

qnameDef :: TElement (Namespace -> String -> Name)
qnameDef = hydraExtrasDefinition "qname" $
  doc "Construct a qualified (dot-separated) name" $
  functionN [namespaceT, stringT, nameT] $
  lambda "ns" $ lambda "name" $
    nom _Name $
      apply Strings.cat $
        list [apply (unwrap _Namespace) (var "ns"), string ".", var "name"]

termArityDef :: TElement (Term -> Int)
termArityDef = hydraExtrasDefinition "termArity" $
  function termT Types.int32 $
  match _Term (Just $ int32 0) [
    TCase _Term_application --> (lambda "x" $ Math.sub @@ var "x" @@ int32 1) <.> ref termArityDef <.> Core.applicationFunction,
    TCase _Term_function --> ref functionArityDef]
    -- Note: ignoring variables which might resolve to functions

typeArityDef :: TElement (Type -> Int)
typeArityDef = hydraExtrasDefinition "typeArity" $
  function typeT Types.int32 $
  match _Type (Just $ int32 0) [
    TCase _Type_annotated --> ref typeArityDef <.> Core.annotatedTypeSubject,
    TCase _Type_application --> ref typeArityDef <.> Core.applicationTypeFunction,
    TCase _Type_lambda --> ref typeArityDef <.> Core.lambdaTypeBody,
    TCase _Type_function --> lambda "f" $
      Math.add @@ (int32 1) @@ (ref typeArityDef @@ (Core.functionTypeCodomain @@ var "f"))]

uncurryTypeDef :: TElement (Type -> [Type])
uncurryTypeDef = hydraExtrasDefinition "uncurryType" $
  function typeT (listT typeT) $
  doc "Uncurry a type expression into a list of types, turning a function type a -> b into cons a (uncurryType b)" $
  lambda "t" ((match _Type (Just $ list [var "t"]) [
    _Type_annotated>>: ref uncurryTypeDef <.> Core.annotatedTypeSubject,
    _Type_application>>: ref uncurryTypeDef <.> Core.applicationTypeFunction,
    _Type_lambda>>: ref uncurryTypeDef <.> Core.lambdaTypeBody,
    _Type_function>>: lambda "ft" $ Lists.cons
      @@ (Core.functionTypeDomain @@ var "ft")
      @@ (ref uncurryTypeDef @@ (Core.functionTypeCodomain @@ var "ft"))]) @@ var "t")

-- hydra/annotations

getAnnotationDef :: TElement (String -> M.Map String Term -> Maybe Term)
getAnnotationDef = hydraExtrasDefinition "getAnnotation" $
  functionN [stringT, kvT, optionalT termT] $
  lambda "key" $ lambda "ann" $
    Maps.lookup @@ var "key" @@ var "ann"
