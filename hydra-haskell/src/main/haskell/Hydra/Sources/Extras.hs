module Hydra.Sources.Extras (hydraExtrasModule) where

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


hydraExtrasDefinition :: String -> Datum a -> Definition a
hydraExtrasDefinition = Definition . fromQname (moduleNamespace hydraExtrasModule)

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
      el typeArityDef,

      el emptyKvDef,
      el getAnnotationDef
--      el getAttrDef
      ]

functionArityDef :: Definition (Function a -> Int)
functionArityDef = hydraExtrasDefinition "functionArity" $
  function (Types.apply (Types.wrap _Function) (Types.var "a")) Types.int32 $
  match _Function Nothing [
    Case _Function_elimination --> constant (int32 1),
    Case _Function_lambda --> (Math.add @@ int32 1) <.> (ref termArityDef <.> project _Lambda _Lambda_body),
    Case _Function_primitive --> constant $
      doc "TODO: This function needs to be monadic, so we can look up the primitive" (int32 42)]

lookupPrimitiveDef :: Definition (Graph a -> Name -> Maybe (Primitive a))
lookupPrimitiveDef = hydraExtrasDefinition "lookupPrimitive" $
  function
    (Types.apply (TypeVariable _Graph) (Types.var "a"))
    (Types.function (TypeVariable _Name) (Types.optional (Types.apply (TypeVariable _Primitive) (Types.var "a")))) $
  lambda "g" $ lambda "name" $
    apply (Maps.lookup @@ var "name") (project _Graph _Graph_primitives @@ var "g")

primitiveArityDef :: Definition (Primitive a -> Int)
primitiveArityDef = hydraExtrasDefinition "primitiveArity" $
  doc "Find the arity (expected number of arguments) of a primitive constant or function" $
  function (Types.apply (Types.wrap _Primitive) (Types.var "a")) Types.int32 $
  (ref typeArityDef <.> (project _Primitive _Primitive_type))

qnameDef :: Definition (Namespace -> String -> Name)
qnameDef = hydraExtrasDefinition "qname" $
  doc "Construct a qualified (dot-separated) name" $
  lambda "ns" $ lambda "name" $
    nom _Name $
      apply Strings.cat $
        list [apply (unwrap _Namespace) (var "ns"), string ".", var "name"]

termArityDef :: Definition (Term a -> Int)
termArityDef = hydraExtrasDefinition "termArity" $
  function (Types.apply (Types.wrap _Term) (Types.var "a")) Types.int32 $
  match _Term (Just $ Terms.int32 0) [
    Case _Term_application --> (lambda "x" $ Math.sub @@ var "x" @@ int32 1) <.> (ref termArityDef <.> (project _Application _Application_function)),
    Case _Term_function --> ref functionArityDef]
    -- Note: ignoring variables which might resolve to functions

typeArityDef :: Definition (Type a -> Int)
typeArityDef = hydraExtrasDefinition "typeArity" $
  function (Types.apply (TypeVariable _Type) (Types.var "a")) Types.int32 $
  match _Type (Just $ Terms.int32 0) [
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
emptyKvDef = hydraExtrasDefinition "emptyKv" $
  record _Kv [fld _Kv_annotations Maps.empty]

getAnnotationDef :: Definition (String -> Kv -> Maybe (Term Kv))
getAnnotationDef = hydraExtrasDefinition "getAnnotation" $
  lambda "key" $ lambda "ann" $
    Maps.lookup @@ var "key" @@ (project _Kv _Kv_annotations @@ var "ann")


--getAttrDef :: Definition (String -> Flow s (Maybe (Term Kv)))
--getAttrDef = hydraExtrasDefinition "getAttr" $
--  lambda "key" $ wrap _Flow $
--    function Types.string (Types.apply (Types.apply (Types.wrap _Flow) (Types.var "s")) (Types.optional $ Types.apply (Types.wrap _Term) (Types.wrap _Kv))) $
--    lambda "s0" $ lambda "t0" $ record _FlowState [
--      fld _FlowState_value (just (Maps.lookup @@ var "key" @@ (project _Trace _Trace_other @@ var "t0"))),
--      fld _FlowState_state $ var "s0",
--      fld _FlowState_trace $ var "t0"]
