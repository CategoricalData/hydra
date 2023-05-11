module Hydra.Sources.Extras where

import Hydra.Kernel
import Hydra.Sources.Compute
import Hydra.Sources.Graph
import Hydra.Sources.Mantle
import Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types


extrasDefinition :: String -> Datum a -> Definition a
extrasDefinition = Definition . fromQname (moduleNamespace hydraExtrasModule)

hydraExtrasModule :: Module Kv
hydraExtrasModule = Module (Namespace "hydra/extras") elements [hydraGraphModule, hydraMantleModule, hydraComputeModule] $
    Just "Basic functions which depend on primitive functions"
  where
    elements = [
      el functionArityDef,
      el lookupPrimitiveDef,
      el primitiveArityDef,
      el qnameDef,
      el termArityDef,
      el testListsDef,
      el typeArityDef,

      el emptyKvDef,
      el getAnnotationDef
--      el getAttrDef
      ]

functionArityDef :: Definition (Function a -> Int)
functionArityDef = extrasDefinition "functionArity" $
  function (Types.apply (Types.wrap _Function) (Types.variable "a")) Types.int32 $
  match _Function Types.int32 Nothing [
    Case _Function_elimination --> constant (int32 1),
    Case _Function_lambda --> (Math.add @@ int32 1) <.> (ref termArityDef <.> project _Lambda _Lambda_body),
    Case _Function_primitive --> constant $
      doc "TODO: This function needs to be monadic, so we can look up the primitive" (int32 42)]

lookupPrimitiveDef :: Definition (Graph a -> Name -> Maybe (Primitive a))
lookupPrimitiveDef = extrasDefinition "lookupPrimitive" $
  function
    (Types.apply (Types.wrap _Graph) (Types.variable "a"))
    (Types.function (Types.wrap _Name) (Types.optional (Types.apply (Types.wrap _Primitive) (Types.variable "a")))) $
  lambda "g" $ lambda "name" $ apply (Maps.lookup @@ var "name") (project _Graph _Graph_primitives @@ var "g")

primitiveArityDef :: Definition (Primitive a -> Int)
primitiveArityDef = extrasDefinition "primitiveArity" $
  doc "Find the arity (expected number of arguments) of a primitive constant or function" $
  function (Types.apply (Types.wrap _Primitive) (Types.variable "a")) Types.int32 $
  (ref typeArityDef <.> (project _Primitive _Primitive_type))

qnameDef :: Definition (Namespace -> String -> Name)
qnameDef = extrasDefinition "qname" $
  doc "Construct a qualified (dot-separated) name" $
  lambda "ns" $
    lambda "name" $
      nom _Name $
        apply Strings.cat $
          list [apply (denom _Namespace) (var "ns"), string ".", var "name"]

termArityDef :: Definition (Term a -> Int)
termArityDef = extrasDefinition "termArity" $
  function (Types.apply (Types.wrap _Term) (Types.variable "a")) Types.int32 $
  match _Term Types.int32 (Just $ Terms.int32 0) [
    Case _Term_application --> (lambda "x" $ Math.sub @@ var "x" @@ int32 1) <.> (ref termArityDef <.> (project _Application _Application_function)),
    Case _Term_function --> ref functionArityDef]
    -- Note: ignoring variables which might resolve to functions

-- TODO: remove once there are other polymorphic functions in use
testListsDef :: Definition ([[a]] -> Int)
testListsDef = extrasDefinition "testLists" $
  doc "TODO: temporary. Just a token polymorphic function for testing" $
  function (Types.list $ Types.list $ Types.variable "a") Types.int32 $
  (lambda "els" (apply Lists.length (apply Lists.concat $ var "els")))

typeArityDef :: Definition (Type a -> Int)
typeArityDef = extrasDefinition "typeArity" $
  function (Types.apply (Types.wrap _Type) (Types.variable "a")) Types.int32 $
  match _Type Types.int32 (Just $ Terms.int32 0) [
    Case _Type_annotated --> ref typeArityDef <.> (project _Annotated _Annotated_subject),
    Case _Type_application --> ref typeArityDef <.> (project _ApplicationType _ApplicationType_function),
    Case _Type_lambda --> ref typeArityDef <.> (project _LambdaType _LambdaType_body),
    Case _Type_function --> lambda "f" $
      Math.add @@ (int32 1) @@ (ref typeArityDef @@ (apply (project _FunctionType _FunctionType_codomain) (var "f")))]


-- hydra/flows

--unexpected :: Show x => String -> x -> Flow s y
--unexpected cat obj = fail $ "expected " ++ cat ++ " but found: " ++ show obj

-- hydra/kv

emptyKvDef :: Definition Kv
emptyKvDef = extrasDefinition "emptyKv" $
  record _Kv [fld _Kv_annotations Maps.empty]

getAnnotationDef :: Definition (String -> Kv -> Maybe (Term Kv))
getAnnotationDef = extrasDefinition "getAnnotation" $
  lambda "key" $ lambda "ann" (Maps.lookup @@ var "key" @@ (project _Kv _Kv_annotations @@ var "ann"))

--getAttrDef :: Definition (String -> Flow s (Maybe (Term Kv)))
--getAttrDef = extrasDefinition "getAttr" $
--  lambda "key" $ wrap _Flow $
--    function Types.string (Types.apply (Types.apply (Types.wrap _Flow) (Types.variable "s")) (Types.optional $ Types.apply (Types.wrap _Term) (Types.wrap _Kv))) $
--    lambda "s0" $ lambda "t0" $ record _FlowState [
--      fld _FlowState_value (just (Maps.lookup @@ var "key" @@ (project _Trace _Trace_other @@ var "t0"))),
--      fld _FlowState_state $ var "s0",
--      fld _FlowState_trace $ var "t0"]