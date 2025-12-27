-- Note: this is an automatically generated file. Do not edit.

-- | Type checking and type reconstruction (type-of) for the results of Hydra unification and inference

module Hydra.Checking where

import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Monads as Monads
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Show.Meta as Meta
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

allEqual :: (Eq t0) => ([t0] -> Bool)
allEqual els = (Logic.ifElse (Lists.null els) True (Lists.foldl (\b -> \t -> Logic.and b (Equality.equal t (Lists.head els))) True (Lists.tail els)))

applyTypeArgumentsToType :: (Typing.TypeContext -> [Core.Type] -> Core.Type -> Compute.Flow t0 Core.Type)
applyTypeArgumentsToType tx typeArgs t =  
  let nonnull = ((\x -> case x of
          Core.TypeForall v1 ->  
            let v = (Core.forallTypeParameter v1)
            in  
              let tbody = (Core.forallTypeBody v1)
              in (applyTypeArgumentsToType tx (Lists.tail typeArgs) (Substitution.substInType (Typing.TypeSubst (Maps.singleton v (Lists.head typeArgs))) tbody))
          _ -> (Flows.fail (Strings.cat [
            "not a forall type: ",
            (Core__.type_ t)]))) t)
  in (Flows.bind (checkTypeVariables tx t) (\_ -> Logic.ifElse (Lists.null typeArgs) (Flows.pure t) nonnull))

checkForUnboundTypeVariables :: (Typing.InferenceContext -> Core.Term -> Compute.Flow t0 ())
checkForUnboundTypeVariables cx term0 =  
  let svars = (Sets.fromList (Maps.keys (Typing.inferenceContextSchemaTypes cx)))
  in  
    let checkRecursive = (\vars -> \trace -> \lbinding -> \term ->  
            let recurse = (checkRecursive vars trace lbinding)
            in  
              let dflt = (Flows.bind (Flows.mapList recurse (Rewriting.subterms term)) (\_ -> Flows.pure ()))
              in  
                let check = (\typ ->  
                        let freevars = (Rewriting.freeVariablesInType typ)
                        in  
                          let badvars = (Sets.difference (Sets.difference freevars vars) svars)
                          in (Logic.ifElse (Sets.null badvars) (Flows.pure ()) (Flows.fail (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "unbound type variables: {" (Strings.intercalate ", " (Lists.map Core.unName (Sets.toList badvars)))) "} in type ") (Core__.type_ typ)) " at path: ") (Strings.intercalate " >> " (Lists.reverse trace))) (Maybes.maybe "none" (\binding -> Strings.cat2 (Strings.cat2 (Strings.cat2 ". bound term = " (Core__.term (Core.bindingTerm binding))) ". bound type = ") (Maybes.maybe "none" Core__.typeScheme (Core.bindingType binding))) lbinding)))))
                in  
                  let checkOptional = (\m -> Flows.bind (Flows.mapMaybe check m) (\_ -> Flows.pure ()))
                  in  
                    let checkOptionalList = (\ml -> Flows.bind (Flows.mapMaybe (\l -> Flows.mapList check l) ml) (\_ -> Flows.pure ()))
                    in ((\x -> case x of
                      Core.TermFunction v1 -> ((\x -> case x of
                        Core.FunctionElimination _ -> dflt
                        Core.FunctionLambda v2 -> (Flows.bind (checkOptional (Core.lambdaDomain v2)) (\_ -> recurse (Core.lambdaBody v2)))
                        _ -> dflt) v1)
                      Core.TermLet v1 ->  
                        let forBinding = (\b ->  
                                let bterm = (Core.bindingTerm b)
                                in  
                                  let newVars = (Maybes.maybe vars (\ts -> Sets.union vars (Sets.fromList (Core.typeSchemeVariables ts))) (Core.bindingType b))
                                  in  
                                    let newTrace = (Lists.cons (Core.unName (Core.bindingName b)) trace)
                                    in (checkRecursive newVars newTrace (Just b) bterm))
                        in (Flows.bind (Flows.mapList forBinding (Core.letBindings v1)) (\_ -> recurse (Core.letBody v1)))
                      Core.TermTypeApplication v1 -> (Flows.bind (check (Core.typeApplicationTermType v1)) (\_ -> recurse (Core.typeApplicationTermBody v1)))
                      Core.TermTypeLambda v1 -> (Flows.bind (check (Core.TypeVariable (Core.typeLambdaParameter v1))) (\_ -> recurse (Core.typeLambdaBody v1)))
                      _ -> dflt) term))
    in (checkRecursive Sets.empty [
      "top level"] Nothing term0)

checkNominalApplication :: (Typing.TypeContext -> Core.Name -> [Core.Type] -> Compute.Flow t0 ())
checkNominalApplication tx tname typeArgs = (Flows.bind (Schemas.requireSchemaType (Typing.typeContextInferenceContext tx) tname) (\schemaType ->  
  let vars = (Core.typeSchemeVariables schemaType)
  in  
    let body = (Core.typeSchemeType schemaType)
    in  
      let varslen = (Lists.length vars)
      in  
        let argslen = (Lists.length typeArgs)
        in (Logic.ifElse (Equality.equal varslen argslen) (Flows.pure ()) (Flows.fail (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "nominal type " (Core.unName tname)) " applied to the wrong number of type arguments: ") "(expected ") (Literals.showInt32 varslen)) " arguments, got ") (Literals.showInt32 argslen)) "): ") (Formatting.showList Core__.type_ typeArgs))))))

checkSameType :: (Typing.TypeContext -> String -> [Core.Type] -> Compute.Flow t0 Core.Type)
checkSameType tx desc types = (Logic.ifElse (typesAllEffectivelyEqual tx types) (Flows.pure (Lists.head types)) (Flows.fail (Strings.cat [
  "unequal types ",
  Formatting.showList Core__.type_ types,
  " in ",
  desc])))

checkType :: (Typing.TypeContext -> Core.Term -> Core.Type -> Compute.Flow t0 ())
checkType tx term typ =  
  let cx = (Typing.typeContextInferenceContext tx)
  in  
    let vars = (Typing.typeContextTypeVariables tx)
    in (Logic.ifElse Constants.debugInference (Flows.bind (typeOf tx [] term) (\t0 -> Logic.ifElse (typesEffectivelyEqual tx t0 typ) (Flows.pure ()) (Flows.fail (Strings.cat [
      "type checking failed: expected ",
      Core__.type_ typ,
      " but found ",
      (Core__.type_ t0)])))) (Flows.pure ()))

checkTypeSubst :: (Typing.InferenceContext -> Typing.TypeSubst -> Compute.Flow t0 Typing.TypeSubst)
checkTypeSubst cx subst =  
  let s = (Typing.unTypeSubst subst)
  in  
    let vars = (Sets.fromList (Maps.keys s))
    in  
      let suspectVars = (Sets.intersection vars (Sets.fromList (Maps.keys (Typing.inferenceContextSchemaTypes cx))))
      in  
        let isNominal = (\ts -> (\x -> case x of
                Core.TypeRecord _ -> True
                Core.TypeUnion _ -> True
                Core.TypeWrap _ -> True
                _ -> False) (Rewriting.deannotateType (Core.typeSchemeType ts)))
        in  
          let badVars = (Sets.fromList (Lists.filter (\v -> Maybes.maybe False isNominal (Lexical.dereferenceSchemaType v (Typing.inferenceContextSchemaTypes cx))) (Sets.toList suspectVars)))
          in  
            let badPairs = (Lists.filter (\p -> Sets.member (Pairs.first p) badVars) (Maps.toList s))
            in  
              let printPair = (\p -> Strings.cat2 (Strings.cat2 (Core.unName (Pairs.first p)) " --> ") (Core__.type_ (Pairs.second p)))
              in (Logic.ifElse (Sets.null badVars) (Flows.pure subst) (Flows.fail (Strings.cat2 (Strings.cat2 "Schema type(s) incorrectly unified: {" (Strings.intercalate ", " (Lists.map printPair badPairs))) "}")))

checkTypeVariables :: (Typing.TypeContext -> Core.Type -> Compute.Flow t0 ())
checkTypeVariables tx typ =  
  let cx = (Typing.typeContextInferenceContext tx)
  in  
    let vars = (Typing.typeContextTypeVariables tx)
    in  
      let dflt = (Flows.bind (Flows.mapList (checkTypeVariables tx) (Rewriting.subtypes typ)) (\_ -> Flows.pure ()))
      in  
        let check = ((\x -> case x of
                Core.TypeForall v1 -> (checkTypeVariables (Typing.TypeContext {
                  Typing.typeContextTypes = (Typing.typeContextTypes tx),
                  Typing.typeContextMetadata = (Typing.typeContextMetadata tx),
                  Typing.typeContextTypeVariables = (Sets.insert (Core.forallTypeParameter v1) vars),
                  Typing.typeContextLambdaVariables = (Typing.typeContextLambdaVariables tx),
                  Typing.typeContextInferenceContext = (Typing.typeContextInferenceContext tx)}) (Core.forallTypeBody v1))
                Core.TypeVariable v1 -> (Logic.ifElse (Sets.member v1 vars) (Flows.pure ()) (Logic.ifElse (Maps.member v1 (Typing.inferenceContextSchemaTypes cx)) (Flows.pure ()) (Flows.fail (Strings.cat [
                  "unbound type variable \"",
                  Core.unName v1,
                  "\" in ",
                  Core__.type_ typ,
                  ". Local variables: {",
                  Strings.intercalate ", " (Lists.map Core.unName (Sets.toList vars)),
                  "}, schema variables: {",
                  Strings.intercalate ", " (Lists.map Core.unName (Maps.keys (Typing.inferenceContextSchemaTypes cx))),
                  "}"]))))
                _ -> dflt) typ)
        in (Monads.withTrace (Strings.cat [
          "checking variables of: ",
          (Core__.type_ typ)]) check)

-- | Convert an inference context to a type environment by converting type schemes to System F types
toFContext :: (Typing.InferenceContext -> M.Map Core.Name Core.Type)
toFContext cx = (Maps.map Schemas.typeSchemeToFType (Typing.inferenceContextDataTypes cx))

-- | Check whether two lists of types are effectively equal, disregarding type aliases
typeListsEffectivelyEqual :: (Typing.TypeContext -> [Core.Type] -> [Core.Type] -> Bool)
typeListsEffectivelyEqual tx tlist1 tlist2 = (Logic.ifElse (Equality.equal (Lists.length tlist1) (Lists.length tlist2)) (Lists.foldl Logic.and True (Lists.zipWith (typesEffectivelyEqual tx) tlist1 tlist2)) False)

typeOf :: (Typing.TypeContext -> [Core.Type] -> Core.Term -> Compute.Flow t0 Core.Type)
typeOf tx typeArgs term =  
  let check = ((\x -> case x of
          Core.TermAnnotated v1 -> (typeOfAnnotatedTerm tx typeArgs v1)
          Core.TermApplication v1 -> (typeOfApplication tx typeArgs v1)
          Core.TermEither v1 -> (typeOfEither tx typeArgs v1)
          Core.TermFunction v1 -> ((\x -> case x of
            Core.FunctionElimination v2 -> ((\x -> case x of
              Core.EliminationRecord v3 -> (typeOfProjection tx typeArgs v3)
              Core.EliminationUnion v3 -> (typeOfCaseStatement tx typeArgs v3)
              Core.EliminationWrap v3 -> (typeOfUnwrap tx typeArgs v3)) v2)
            Core.FunctionLambda v2 -> (typeOfLambda tx typeArgs v2)
            Core.FunctionPrimitive v2 -> (typeOfPrimitive tx typeArgs v2)) v1)
          Core.TermLet v1 -> (typeOfLet tx typeArgs v1)
          Core.TermList v1 -> (typeOfList tx typeArgs v1)
          Core.TermLiteral v1 -> (typeOfLiteral tx typeArgs v1)
          Core.TermMap v1 -> (typeOfMap tx typeArgs v1)
          Core.TermMaybe v1 -> (typeOfMaybe tx typeArgs v1)
          Core.TermPair v1 -> (typeOfPair tx typeArgs v1)
          Core.TermRecord v1 -> (typeOfRecord tx typeArgs v1)
          Core.TermSet v1 -> (typeOfSet tx typeArgs v1)
          Core.TermTypeApplication v1 -> (typeOfTypeApplication tx typeArgs v1)
          Core.TermTypeLambda v1 -> (typeOfTypeLambda tx typeArgs v1)
          Core.TermUnion v1 -> (typeOfInjection tx typeArgs v1)
          Core.TermUnit -> (typeOfUnit tx typeArgs)
          Core.TermVariable v1 -> (typeOfVariable tx typeArgs v1)
          Core.TermWrap v1 -> (typeOfWrappedTerm tx typeArgs v1)
          _ -> (Flows.fail (Strings.cat [
            "unsupported term variant in typeOf: ",
            (Meta.termVariant (Reflect.termVariant term))]))) term)
  in (Monads.withTrace "typeOf" check)

typeOfAnnotatedTerm :: (Typing.TypeContext -> [Core.Type] -> Core.AnnotatedTerm -> Compute.Flow t0 Core.Type)
typeOfAnnotatedTerm tx typeArgs at = (typeOf tx typeArgs (Core.annotatedTermBody at))

typeOfApplication :: (Typing.TypeContext -> [Core.Type] -> Core.Application -> Compute.Flow t0 Core.Type)
typeOfApplication tx typeArgs app =  
  let fun = (Core.applicationFunction app)
  in  
    let arg = (Core.applicationArgument app)
    in  
      let tryType = (\tfun -> \targ -> (\x -> case x of
              Core.TypeForall v1 -> (tryType (Core.forallTypeBody v1) targ)
              Core.TypeFunction v1 ->  
                let dom = (Core.functionTypeDomain v1)
                in  
                  let cod = (Core.functionTypeCodomain v1)
                  in (Logic.ifElse (typesEffectivelyEqual tx dom targ) (Flows.pure cod) (Flows.fail (Strings.cat [
                    "in application, expected ",
                    Core__.type_ dom,
                    " but found ",
                    (Core__.type_ targ)])))
              Core.TypeVariable _ -> (Flows.map (\x -> Core.TypeVariable x) Schemas.freshName)
              _ -> (Flows.fail (Strings.cat [
                "left hand side of application (",
                Core__.term fun,
                ") is not function-typed (",
                Core__.type_ tfun,
                ")",
                ". types: ",
                (Strings.intercalate ", " (Lists.map (\p -> Strings.cat [
                  Core.unName (Pairs.first p),
                  ": ",
                  (Core__.type_ (Pairs.second p))]) (Maps.toList (Typing.typeContextTypes tx))))]))) tfun)
      in (Flows.bind (typeOf tx [] fun) (\tfun -> Flows.bind (checkTypeVariables tx tfun) (\_ -> Flows.bind (typeOf tx [] arg) (\targ -> Flows.bind (checkTypeVariables tx targ) (\_ -> Flows.bind (tryType tfun targ) (\t -> applyTypeArgumentsToType tx typeArgs t))))))

typeOfCaseStatement :: (Typing.TypeContext -> [Core.Type] -> Core.CaseStatement -> Compute.Flow t0 Core.Type)
typeOfCaseStatement tx typeArgs cs =  
  let tname = (Core.caseStatementTypeName cs)
  in  
    let dflt = (Core.caseStatementDefault cs)
    in  
      let cases = (Core.caseStatementCases cs)
      in  
        let cterms = (Lists.map Core.fieldTerm cases)
        in (Flows.bind (Flows.mapMaybe (\e -> typeOf tx [] e) dflt) (\tdflt -> Flows.bind (Flows.mapList (\e -> typeOf tx [] e) cterms) (\tcterms -> Flows.bind (Flows.mapList (\t -> Flows.map Core.functionTypeCodomain (Core_.functionType t)) tcterms) (\fcods ->  
          let cods = (Maybes.cat (Lists.cons tdflt (Lists.map Maybes.pure fcods)))
          in (Flows.bind (checkSameType tx "case branches" cods) (\cod -> Flows.pure (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Schemas.nominalApplication tname typeArgs),
            Core.functionTypeCodomain = cod}))))))))

typeOfEither :: (Typing.TypeContext -> [Core.Type] -> Either Core.Term Core.Term -> Compute.Flow t0 Core.Type)
typeOfEither tx typeArgs et =  
  let checkLength =  
          let n = (Lists.length typeArgs)
          in (Logic.ifElse (Equality.equal n 2) (Flows.pure ()) (Flows.fail (Strings.cat2 "either type requires 2 type arguments, got " (Literals.showInt32 n))))
  in (Flows.bind checkLength (\_ -> Eithers.either (\leftTerm -> Flows.bind (typeOf tx [] leftTerm) (\leftType -> Flows.bind (checkTypeVariables tx leftType) (\_ -> Flows.pure (Core.TypeEither (Core.EitherType {
    Core.eitherTypeLeft = leftType,
    Core.eitherTypeRight = (Lists.at 1 typeArgs)}))))) (\rightTerm -> Flows.bind (typeOf tx [] rightTerm) (\rightType -> Flows.bind (checkTypeVariables tx rightType) (\_ -> Flows.pure (Core.TypeEither (Core.EitherType {
    Core.eitherTypeLeft = (Lists.at 0 typeArgs),
    Core.eitherTypeRight = rightType}))))) et))

typeOfInjection :: (Typing.TypeContext -> [Core.Type] -> Core.Injection -> Compute.Flow t0 Core.Type)
typeOfInjection tx typeArgs injection =  
  let tname = (Core.injectionTypeName injection)
  in  
    let field = (Core.injectionField injection)
    in  
      let fname = (Core.fieldName field)
      in  
        let fterm = (Core.fieldTerm field)
        in (Flows.bind (Schemas.requireSchemaType (Typing.typeContextInferenceContext tx) tname) (\schemaType ->  
          let svars = (Core.typeSchemeVariables schemaType)
          in  
            let sbody = (Core.typeSchemeType schemaType)
            in (Flows.bind (Core_.unionType tname sbody) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp -> Flows.pure (Schemas.nominalApplication tname typeArgs))))))

typeOfLambda :: (Typing.TypeContext -> [Core.Type] -> Core.Lambda -> Compute.Flow t0 Core.Type)
typeOfLambda tx typeArgs l =  
  let v = (Core.lambdaParameter l)
  in  
    let mdom = (Core.lambdaDomain l)
    in  
      let body = (Core.lambdaBody l)
      in (Flows.bind (Maybes.maybe (Flows.fail "untyped lambda") (\dom -> Flows.bind (checkTypeVariables tx dom) (\_ ->  
        let types2 = (Maps.insert v dom (Typing.typeContextTypes tx))
        in (Flows.bind (typeOf (Typing.TypeContext {
          Typing.typeContextTypes = types2,
          Typing.typeContextMetadata = (Typing.typeContextMetadata tx),
          Typing.typeContextTypeVariables = (Typing.typeContextTypeVariables tx),
          Typing.typeContextLambdaVariables = (Typing.typeContextLambdaVariables tx),
          Typing.typeContextInferenceContext = (Typing.typeContextInferenceContext tx)}) [] body) (\cod -> Flows.bind (checkTypeVariables tx cod) (\_ -> Flows.pure (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = dom,
          Core.functionTypeCodomain = cod}))))))) mdom) (\tbody -> applyTypeArgumentsToType tx typeArgs tbody))

typeOfLet :: (Typing.TypeContext -> [Core.Type] -> Core.Let -> Compute.Flow t0 Core.Type)
typeOfLet tx typeArgs letTerm =  
  let bs = (Core.letBindings letTerm)
  in  
    let body = (Core.letBody letTerm)
    in  
      let bnames = (Lists.map Core.bindingName bs)
      in  
        let bterms = (Lists.map Core.bindingTerm bs)
        in  
          let bindingType = (\b -> Maybes.maybe (Flows.fail (Strings.cat [
                  "untyped let binding: ",
                  (Core__.binding b)])) (\ts -> Flows.pure (Schemas.typeSchemeToFType ts)) (Core.bindingType b))
          in (Flows.bind (Flows.mapList bindingType bs) (\btypes ->  
            let tx2 = Typing.TypeContext {
                    Typing.typeContextTypes = (Maps.union (Maps.fromList (Lists.zip bnames btypes)) (Typing.typeContextTypes tx)),
                    Typing.typeContextMetadata = (Typing.typeContextMetadata tx),
                    Typing.typeContextTypeVariables = (Typing.typeContextTypeVariables tx),
                    Typing.typeContextLambdaVariables = (Typing.typeContextLambdaVariables tx),
                    Typing.typeContextInferenceContext = (Typing.typeContextInferenceContext tx)}
            in (Flows.bind (Flows.mapList (typeOf tx2 []) bterms) (\typeofs -> Flows.bind (Flows.mapList (checkTypeVariables tx) btypes) (\_ -> Flows.bind (Flows.mapList (checkTypeVariables tx) typeofs) (\_ -> Flows.bind (Logic.ifElse (typeListsEffectivelyEqual tx typeofs btypes) (typeOf tx2 [] body) (Flows.fail (Strings.cat [
              "binding types disagree: ",
              Formatting.showList Core__.type_ btypes,
              " and ",
              Formatting.showList Core__.type_ typeofs,
              " from terms: ",
              (Formatting.showList Core__.term bterms)]))) (\t -> applyTypeArgumentsToType tx typeArgs t)))))))

typeOfList :: (Typing.TypeContext -> [Core.Type] -> [Core.Term] -> Compute.Flow t0 Core.Type)
typeOfList tx typeArgs els = (Logic.ifElse (Lists.null els) (Logic.ifElse (Equality.equal (Lists.length typeArgs) 1) (Flows.pure (Core.TypeList (Lists.head typeArgs))) (Flows.fail "list type applied to more or less than one argument")) (Flows.bind (Flows.mapList (typeOf tx []) els) (\eltypes -> Flows.bind (checkSameType tx "list elements" eltypes) (\unifiedType -> Flows.bind (checkTypeVariables tx unifiedType) (\_ -> Flows.pure (Core.TypeList unifiedType))))))

typeOfLiteral :: (Typing.TypeContext -> [Core.Type] -> Core.Literal -> Compute.Flow t0 Core.Type)
typeOfLiteral tx typeArgs lit =  
  let t = (Core.TypeLiteral (Reflect.literalType lit))
  in (applyTypeArgumentsToType tx typeArgs t)

typeOfMap :: (Typing.TypeContext -> [Core.Type] -> M.Map Core.Term Core.Term -> Compute.Flow t0 Core.Type)
typeOfMap tx typeArgs m =  
  let nonnull =  
          let pairs = (Maps.toList m)
          in (Flows.bind (Flows.bind (Flows.mapList (typeOf tx []) (Lists.map Pairs.first pairs)) (checkSameType tx "map keys")) (\kt -> Flows.bind (Flows.bind (Flows.mapList (typeOf tx []) (Lists.map Pairs.second pairs)) (checkSameType tx "map values")) (\vt -> Flows.bind (checkTypeVariables tx kt) (\_ -> Flows.bind (checkTypeVariables tx vt) (\_ -> applyTypeArgumentsToType tx typeArgs (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = kt,
            Core.mapTypeValues = vt})))))))
  in (Logic.ifElse (Maps.null m) (Logic.ifElse (Equality.equal (Lists.length typeArgs) 2) (Flows.pure (Core.TypeMap (Core.MapType {
    Core.mapTypeKeys = (Lists.at 0 typeArgs),
    Core.mapTypeValues = (Lists.at 1 typeArgs)}))) (Flows.fail "map type applied to more or less than two arguments")) nonnull)

typeOfMaybe :: (Typing.TypeContext -> [Core.Type] -> Maybe Core.Term -> Compute.Flow t0 Core.Type)
typeOfMaybe tx typeArgs mt =  
  let forNothing =  
          let n = (Lists.length typeArgs)
          in (Logic.ifElse (Equality.equal n 1) (Flows.pure (Core.TypeMaybe (Lists.head typeArgs))) (Flows.fail (Strings.cat2 (Strings.cat2 "optional type applied to " (Literals.showInt32 n)) " argument(s). Expected 1.")))
  in  
    let forJust = (\term -> Flows.bind (Flows.bind (typeOf tx [] term) (\termType -> Flows.bind (checkTypeVariables tx termType) (\_ -> Flows.pure (Core.TypeMaybe termType)))) (\t -> applyTypeArgumentsToType tx typeArgs t))
    in (Maybes.maybe forNothing forJust mt)

typeOfPair :: (Typing.TypeContext -> [Core.Type] -> (Core.Term, Core.Term) -> Compute.Flow t0 Core.Type)
typeOfPair tx typeArgs p =  
  let checkLength =  
          let n = (Lists.length typeArgs)
          in (Logic.ifElse (Equality.equal n 2) (Flows.pure ()) (Flows.fail (Strings.cat2 "pair type requires 2 type arguments, got " (Literals.showInt32 n))))
  in (Flows.bind checkLength (\_ ->  
    let pairFst = (Pairs.first p)
    in  
      let pairSnd = (Pairs.second p)
      in (Flows.bind (typeOf tx [] pairFst) (\firstType -> Flows.bind (checkTypeVariables tx firstType) (\_ -> Flows.bind (typeOf tx [] pairSnd) (\secondType -> Flows.bind (checkTypeVariables tx secondType) (\_ -> Flows.pure (Core.TypePair (Core.PairType {
        Core.pairTypeFirst = firstType,
        Core.pairTypeSecond = secondType})))))))))

typeOfPrimitive :: (Typing.TypeContext -> [Core.Type] -> Core.Name -> Compute.Flow t0 Core.Type)
typeOfPrimitive tx typeArgs name = (Flows.bind (Maybes.maybe (Flows.fail (Strings.cat [
  "no such primitive: ",
  (Core.unName name)])) Schemas.instantiateTypeScheme (Maps.lookup name (Typing.inferenceContextPrimitiveTypes (Typing.typeContextInferenceContext tx)))) (\ts ->  
  let t = (Schemas.typeSchemeToFType ts)
  in (applyTypeArgumentsToType tx typeArgs t)))

typeOfProjection :: (Typing.TypeContext -> [Core.Type] -> Core.Projection -> Compute.Flow t0 Core.Type)
typeOfProjection tx typeArgs p =  
  let tname = (Core.projectionTypeName p)
  in  
    let fname = (Core.projectionField p)
    in (Flows.bind (Schemas.requireSchemaType (Typing.typeContextInferenceContext tx) tname) (\schemaType ->  
      let svars = (Core.typeSchemeVariables schemaType)
      in  
        let sbody = (Core.typeSchemeType schemaType)
        in (Flows.bind (Core_.recordType tname sbody) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp ->  
          let subst = (Typing.TypeSubst (Maps.fromList (Lists.zip svars typeArgs)))
          in  
            let sftyp = (Substitution.substInType subst ftyp)
            in (Flows.pure (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Schemas.nominalApplication tname typeArgs),
              Core.functionTypeCodomain = sftyp}))))))))

typeOfRecord :: (Typing.TypeContext -> [Core.Type] -> Core.Record -> Compute.Flow t0 Core.Type)
typeOfRecord tx typeArgs record =  
  let tname = (Core.recordTypeName record)
  in  
    let fields = (Core.recordFields record)
    in (Flows.bind (Flows.mapList (typeOf tx []) (Lists.map Core.fieldTerm fields)) (\ftypes -> Flows.bind (Flows.mapList (checkTypeVariables tx) ftypes) (\_ -> Flows.pure (Schemas.nominalApplication tname typeArgs))))

typeOfSet :: (Typing.TypeContext -> [Core.Type] -> S.Set Core.Term -> Compute.Flow t0 Core.Type)
typeOfSet tx typeArgs els = (Logic.ifElse (Sets.null els) (Logic.ifElse (Equality.equal (Lists.length typeArgs) 1) (Flows.pure (Core.TypeSet (Lists.head typeArgs))) (Flows.fail "set type applied to more or less than one argument")) (Flows.bind (Flows.mapList (typeOf tx []) (Sets.toList els)) (\eltypes -> Flows.bind (checkSameType tx "set elements" eltypes) (\unifiedType -> Flows.bind (checkTypeVariables tx unifiedType) (\_ -> Flows.pure (Core.TypeSet unifiedType))))))

typeOfTypeApplication :: (Typing.TypeContext -> [Core.Type] -> Core.TypeApplicationTerm -> Compute.Flow t0 Core.Type)
typeOfTypeApplication tx typeArgs tyapp =  
  let body = (Core.typeApplicationTermBody tyapp)
  in  
    let t = (Core.typeApplicationTermType tyapp)
    in (typeOf tx (Lists.cons t typeArgs) body)

typeOfTypeLambda :: (Typing.TypeContext -> [Core.Type] -> Core.TypeLambda -> Compute.Flow t0 Core.Type)
typeOfTypeLambda tx typeArgs tl =  
  let v = (Core.typeLambdaParameter tl)
  in  
    let body = (Core.typeLambdaBody tl)
    in  
      let vars = (Typing.typeContextTypeVariables tx)
      in  
        let tx2 = Typing.TypeContext {
                Typing.typeContextTypes = (Typing.typeContextTypes tx),
                Typing.typeContextMetadata = (Typing.typeContextMetadata tx),
                Typing.typeContextTypeVariables = (Sets.insert v vars),
                Typing.typeContextLambdaVariables = (Typing.typeContextLambdaVariables tx),
                Typing.typeContextInferenceContext = (Typing.typeContextInferenceContext tx)}
        in (Flows.bind (typeOf tx2 [] body) (\t1 -> Flows.bind (checkTypeVariables tx2 t1) (\_ -> applyTypeArgumentsToType tx typeArgs (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = v,
          Core.forallTypeBody = t1})))))

typeOfUnit :: (Typing.TypeContext -> [Core.Type] -> Compute.Flow t0 Core.Type)
typeOfUnit tx typeArgs = (applyTypeArgumentsToType tx typeArgs Core.TypeUnit)

typeOfUnwrap :: (Typing.TypeContext -> [Core.Type] -> Core.Name -> Compute.Flow t0 Core.Type)
typeOfUnwrap tx typeArgs tname = (Flows.bind (Schemas.requireSchemaType (Typing.typeContextInferenceContext tx) tname) (\schemaType ->  
  let svars = (Core.typeSchemeVariables schemaType)
  in  
    let sbody = (Core.typeSchemeType schemaType)
    in (Flows.bind (Core_.wrappedType tname sbody) (\wrapped ->  
      let subst = (Typing.TypeSubst (Maps.fromList (Lists.zip svars typeArgs)))
      in  
        let swrapped = (Substitution.substInType subst wrapped)
        in (Flows.pure (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Schemas.nominalApplication tname typeArgs),
          Core.functionTypeCodomain = swrapped})))))))

typeOfVariable :: (Typing.TypeContext -> [Core.Type] -> Core.Name -> Compute.Flow t0 Core.Type)
typeOfVariable tx typeArgs name = (Flows.bind (Maybes.maybe (Flows.fail (Strings.cat [
  "unbound variable: ",
  Core.unName name,
  ". Variables: {",
  Strings.intercalate ", " (Lists.map Core.unName (Maps.keys (Typing.typeContextTypes tx))),
  "}"])) Schemas.instantiateType (Maps.lookup name (Typing.typeContextTypes tx))) (\t -> applyTypeArgumentsToType tx typeArgs t))

typeOfWrappedTerm :: (Typing.TypeContext -> [Core.Type] -> Core.WrappedTerm -> Compute.Flow t0 Core.Type)
typeOfWrappedTerm tx typeArgs wt =  
  let tname = (Core.wrappedTermTypeName wt)
  in  
    let body = (Core.wrappedTermBody wt)
    in (Flows.bind (typeOf tx [] body) (\btype -> Flows.bind (checkTypeVariables tx btype) (\_ -> Flows.pure (Schemas.nominalApplication tname typeArgs))))

-- | Check if a type contains any type variable from the current scope
containsInScopeTypeVars :: (Typing.TypeContext -> Core.Type -> Bool)
containsInScopeTypeVars tx t =  
  let vars = (Typing.typeContextTypeVariables tx)
  in  
    let freeVars = (Rewriting.freeVariablesInTypeSimple t)
    in (Logic.not (Sets.null (Sets.intersection vars freeVars)))

-- | Check whether a list of types are effectively equal, disregarding type aliases
typesAllEffectivelyEqual :: (Typing.TypeContext -> [Core.Type] -> Bool)
typesAllEffectivelyEqual tx tlist =  
  let types = (Typing.inferenceContextSchemaTypes (Typing.typeContextInferenceContext tx))
  in (allEqual (Lists.map (Rewriting.replaceTypedefs types) tlist))

-- | Check whether two types are effectively equal, disregarding type aliases, forall quantifiers, and treating in-scope type variables as wildcards
typesEffectivelyEqual :: (Typing.TypeContext -> Core.Type -> Core.Type -> Bool)
typesEffectivelyEqual tx t1 t2 = (Logic.or (containsInScopeTypeVars tx t1) (Logic.or (containsInScopeTypeVars tx t2) (typesAllEffectivelyEqual tx [
  Schemas.fullyStripType t1,
  (Schemas.fullyStripType t2)])))
