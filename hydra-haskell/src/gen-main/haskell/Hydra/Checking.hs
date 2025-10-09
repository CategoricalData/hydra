-- | Type checking and type reconstruction (type-of) for the results of Hydra unification and inference

module Hydra.Checking where

import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Extract.Core as Core_
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Show.Mantle as Mantle
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import qualified Hydra.Unification as Unification
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

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
                          in (Logic.ifElse (Sets.null badvars) (Flows.pure ()) (Flows.fail (Strings.cat [
                            Strings.cat [
                              Strings.cat [
                                Strings.cat [
                                  Strings.cat [
                                    Strings.cat [
                                      "unbound type variables: {",
                                      (Strings.intercalate ", " (Lists.map Core.unName (Sets.toList badvars)))],
                                    "} in type "],
                                  (Core__.type_ typ)],
                                " at path: "],
                              (Strings.intercalate " >> " (Lists.reverse trace))],
                            (Optionals.maybe "none" (\binding -> Strings.cat [
                              Strings.cat [
                                Strings.cat [
                                  ". bound term = ",
                                  (Core__.term (Core.bindingTerm binding))],
                                ". bound type = "],
                              (Optionals.maybe "none" Core__.typeScheme (Core.bindingType binding))]) lbinding)]))))
                in  
                  let checkOptional = (\m -> Flows.bind (Flows.mapOptional check m) (\_ -> Flows.pure ()))
                  in  
                    let checkOptionalList = (\ml -> Flows.bind (Flows.mapOptional (\l -> Flows.mapList check l) ml) (\_ -> Flows.pure ()))
                    in ((\x -> case x of
                      Core.TermFunction v1 -> ((\x -> case x of
                        Core.FunctionElimination v2 -> ((\x -> case x of
                          Core.EliminationProduct v3 -> (checkOptionalList (Core.tupleProjectionDomain v3))
                          _ -> dflt) v2)
                        Core.FunctionLambda v2 -> (Flows.bind (checkOptional (Core.lambdaDomain v2)) (\_ -> recurse (Core.lambdaBody v2)))
                        _ -> dflt) v1)
                      Core.TermLet v1 ->  
                        let forBinding = (\b ->  
                                let bterm = (Core.bindingTerm b)
                                in  
                                  let newVars = (Optionals.maybe vars (\ts -> Sets.union vars (Sets.fromList (Core.typeSchemeVariables ts))) (Core.bindingType b))
                                  in  
                                    let newTrace = (Lists.cons (Core.unName (Core.bindingName b)) trace)
                                    in (checkRecursive newVars newTrace (Just b) bterm))
                        in (Flows.bind (Flows.mapList forBinding (Core.letBindings v1)) (\_ -> recurse (Core.letEnvironment v1)))
                      Core.TermTypeApplication v1 -> (Flows.bind (check (Core.typedTermType v1)) (\_ -> recurse (Core.typedTermTerm v1)))
                      Core.TermTypeLambda v1 -> (Flows.bind (check (Core.TypeVariable (Core.typeLambdaParameter v1))) (\_ -> recurse (Core.typeLambdaBody v1)))
                      _ -> dflt) term))
    in (checkRecursive Sets.empty [
      "top level"] Nothing term0)

checkSameType :: (String -> [Core.Type] -> Compute.Flow t0 Core.Type)
checkSameType desc types =  
  let h = (Lists.head types)
  in  
    let allEqual = (Lists.foldl (\b -> \t -> Logic.and b (Equality.equal t h)) True types)
    in (Logic.ifElse allEqual (Flows.pure h) (Flows.fail (Strings.cat [
      "unequal types ",
      Formatting.showList Core__.type_ types,
      " in ",
      desc])))

checkType :: (S.Set Core.Name -> Typing.InferenceContext -> Core.Type -> Core.Term -> Compute.Flow t0 ())
checkType k g t e = (Logic.ifElse Constants.debugInference (Flows.bind (typeOfInternal g k (toFContext g) [] e) (\t0 -> Logic.ifElse (Equality.equal t0 t) (Flows.pure ()) (Flows.fail (Strings.cat [
  "type checking failed: expected ",
  Core__.type_ t,
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
          let badVars = (Sets.fromList (Lists.filter (\v -> Optionals.maybe False isNominal (Lexical.dereferenceSchemaType v (Typing.inferenceContextSchemaTypes cx))) (Sets.toList suspectVars)))
          in  
            let badPairs = (Lists.filter (\p -> Sets.member (fst p) badVars) (Maps.toList s))
            in  
              let printPair = (\p -> Strings.cat [
                      Strings.cat [
                        Core.unName (fst p),
                        " --> "],
                      (Core__.type_ (snd p))])
              in (Logic.ifElse (Sets.null badVars) (Flows.pure subst) (Flows.fail (Strings.cat [
                Strings.cat [
                  "Schema type(s) incorrectly unified: {",
                  (Strings.intercalate ", " (Lists.map printPair badPairs))],
                "}"])))

checkTypeVariables :: (Typing.InferenceContext -> S.Set Core.Name -> Core.Type -> Compute.Flow t0 ())
checkTypeVariables cx tyvars typ = (Monads.withTrace (Strings.cat [
  "checking variables of: ",
  (Core__.type_ typ)]) ((\x -> case x of
  Core.TypeForall v1 -> (checkTypeVariables cx (Sets.insert (Core.forallTypeParameter v1) tyvars) (Core.forallTypeBody v1))
  Core.TypeVariable v1 -> (Logic.ifElse (Sets.member v1 tyvars) (Flows.pure ()) (Logic.ifElse (Maps.member v1 (Typing.inferenceContextSchemaTypes cx)) (Flows.pure ()) (Flows.fail (Strings.cat [
    "unbound type variable \"",
    Core.unName v1,
    "\" in ",
    (Core__.type_ typ)]))))
  _ -> (Flows.bind (Flows.mapList (checkTypeVariables cx tyvars) (Rewriting.subtypes typ)) (\result -> Flows.pure ()))) typ))

-- | Convert inference context to type context
toFContext :: (Typing.InferenceContext -> M.Map Core.Name Core.Type)
toFContext cx = (Maps.map Schemas.typeSchemeToFType (Typing.inferenceContextDataTypes cx))

typeOf :: (Typing.TypeContext -> Core.Term -> Compute.Flow t0 Core.Type)
typeOf tcontext term = (typeOfInternal (Typing.typeContextInferenceContext tcontext) (Typing.typeContextVariables tcontext) (Typing.typeContextTypes tcontext) [] term)

typeOfInternal :: (Typing.InferenceContext -> S.Set Core.Name -> M.Map Core.Name Core.Type -> [Core.Type] -> Core.Term -> Compute.Flow t0 Core.Type)
typeOfInternal cx vars types apptypes term =  
  let checkApp = (\e -> Logic.ifElse (Lists.null apptypes) e ( 
          let app = (\t -> \apptypes -> Logic.ifElse (Lists.null apptypes) (Flows.pure t) ((\x -> case x of
                  Core.TypeForall v1 ->  
                    let v = (Core.forallTypeParameter v1)
                    in  
                      let t2 = (Core.forallTypeBody v1)
                      in (app (Substitution.substInType (Typing.TypeSubst (Maps.singleton v (Lists.head apptypes))) t2) (Lists.tail apptypes))
                  _ -> (Flows.fail (Strings.cat [
                    "not a forall type: ",
                    Core__.type_ t,
                    " in ",
                    (Core__.term term)]))) t))
          in (Flows.bind (typeOfInternal cx vars types [] term) (\t1 -> Flows.bind (checkTypeVariables cx vars t1) (\_ -> app t1 apptypes)))))
  in (Monads.withTrace (Strings.cat [
    "checking type of: ",
    Core__.term term,
    " (vars: ",
    Formatting.showList Core.unName (Sets.toList vars),
    ", apptypes: ",
    Formatting.showList Core__.type_ apptypes,
    ", types: ",
    Formatting.showList Core.unName (Maps.keys types),
    ")"]) ((\x -> case x of
    Core.TermAnnotated v1 -> (checkApp ( 
      let term1 = (Core.annotatedTermSubject v1)
      in (typeOfInternal cx vars types apptypes term1)))
    Core.TermApplication v1 -> (checkApp ( 
      let a = (Core.applicationFunction v1)
      in  
        let b = (Core.applicationArgument v1)
        in (Flows.bind (typeOfInternal cx vars types [] a) (\t1 -> Flows.bind (typeOfInternal cx vars types [] b) (\t2 -> Flows.bind (checkTypeVariables cx vars t1) (\_ -> Flows.bind (checkTypeVariables cx vars t2) (\_ ->  
          let tryType = (\t -> (\x -> case x of
                  Core.TypeForall v2 -> (tryType (Core.forallTypeBody v2))
                  Core.TypeFunction v2 ->  
                    let p = (Core.functionTypeDomain v2)
                    in  
                      let q = (Core.functionTypeCodomain v2)
                      in (Logic.ifElse (Equality.equal p t2) (Flows.pure q) (Flows.fail (Strings.cat [
                        "expected ",
                        Core__.type_ p,
                        " in ",
                        Core__.term term,
                        " but found ",
                        (Core__.type_ t2)])))
                  _ -> (Flows.fail (Strings.cat [
                    "left hand side of application ",
                    Core__.term term,
                    " is not a function type: ",
                    (Core__.type_ t)]))) t)
          in (tryType t1))))))))
    Core.TermFunction v1 -> ((\x -> case x of
      Core.FunctionElimination v2 -> ((\x -> case x of
        Core.EliminationProduct v3 -> (checkApp ( 
          let index = (Core.tupleProjectionIndex v3)
          in  
            let arity = (Core.tupleProjectionArity v3)
            in  
              let mtypes = (Core.tupleProjectionDomain v3)
              in (Optionals.maybe (Flows.fail (Strings.cat [
                "untyped tuple projection: ",
                (Core__.term term)])) (\types -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) types) (\_ -> Flows.pure (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeProduct types),
                Core.functionTypeCodomain = (Lists.at index types)})))) mtypes)))
        Core.EliminationRecord v3 ->  
          let tname = (Core.projectionTypeName v3)
          in  
            let fname = (Core.projectionField v3)
            in (Flows.bind (Schemas.requireSchemaType cx tname) (\schemaType ->  
              let svars = (Core.typeSchemeVariables schemaType)
              in  
                let stype = (Core.typeSchemeType schemaType)
                in (Flows.bind (Core_.recordType tname stype) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp ->  
                  let subst = (Typing.TypeSubst (Maps.fromList (Lists.zip svars apptypes)))
                  in  
                    let sftyp = (Substitution.substInType subst ftyp)
                    in (Flows.pure (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Schemas.nominalApplication tname apptypes),
                      Core.functionTypeCodomain = sftyp}))))))))
        Core.EliminationUnion v3 ->  
          let tname = (Core.caseStatementTypeName v3)
          in  
            let dflt = (Core.caseStatementDefault v3)
            in  
              let cases = (Core.caseStatementCases v3)
              in  
                let cterms = (Lists.map Core.fieldTerm cases)
                in (Flows.bind (Schemas.requireSchemaType cx tname) (\schemaType ->  
                  let svars = (Core.typeSchemeVariables schemaType)
                  in  
                    let stype = (Core.typeSchemeType schemaType)
                    in (Flows.bind (Core_.unionType tname stype) (\sfields -> Flows.bind (Flows.mapOptional (\e -> typeOfInternal cx vars types [] e) dflt) (\tdflt -> Flows.bind (Flows.mapList (\e -> typeOfInternal cx vars types [] e) cterms) (\tcterms -> Flows.bind (Flows.mapList (\t -> Flows.map Core.functionTypeCodomain (Core_.functionType t)) tcterms) (\cods ->  
                      let ts = (Optionals.cat (Lists.cons tdflt (Lists.map Optionals.pure cods)))
                      in (Flows.bind (checkSameType "case branches" ts) (\cod ->  
                        let subst = (Typing.TypeSubst (Maps.fromList (Lists.zip svars apptypes)))
                        in  
                          let scod = (Substitution.substInType subst cod)
                          in (Flows.pure (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Schemas.nominalApplication tname apptypes),
                            Core.functionTypeCodomain = scod}))))))))))))
        Core.EliminationWrap v3 -> (Flows.bind (Schemas.requireSchemaType cx v3) (\schemaType ->  
          let svars = (Core.typeSchemeVariables schemaType)
          in  
            let stype = (Core.typeSchemeType schemaType)
            in (Flows.bind (Core_.wrappedType v3 stype) (\wrapped ->  
              let subst = (Typing.TypeSubst (Maps.fromList (Lists.zip svars apptypes)))
              in  
                let swrapped = (Substitution.substInType subst wrapped)
                in (Flows.pure (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Schemas.nominalApplication v3 apptypes),
                  Core.functionTypeCodomain = swrapped})))))))) v2)
      Core.FunctionLambda v2 -> (checkApp ( 
        let x = (Core.lambdaParameter v2)
        in  
          let mt = (Core.lambdaDomain v2)
          in  
            let e = (Core.lambdaBody v2)
            in (Optionals.maybe (Flows.fail (Strings.cat [
              "untyped lambda: ",
              (Core__.term term)])) (\t -> Flows.bind (checkTypeVariables cx vars t) (\_ -> Flows.bind (typeOfInternal cx vars (Maps.insert x t types) [] e) (\t1 -> Flows.bind (checkTypeVariables cx vars t1) (\_ -> Flows.pure (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = t,
              Core.functionTypeCodomain = t1})))))) mt)))
      Core.FunctionPrimitive v2 -> (checkApp ( 
        let ts = (Optionals.maybe (Flows.fail (Strings.cat [
                "no such primitive: ",
                (Core.unName v2)])) Flows.pure (Maps.lookup v2 (Typing.inferenceContextPrimitiveTypes cx)))
        in (Flows.map Schemas.typeSchemeToFType ts)))) v1)
    Core.TermLet v1 -> (checkApp ( 
      let bs = (Core.letBindings v1)
      in  
        let body = (Core.letEnvironment v1)
        in  
          let bnames = (Lists.map Core.bindingName bs)
          in  
            let bterms = (Lists.map Core.bindingTerm bs)
            in  
              let btypeOf = (\b -> Optionals.maybe (Flows.fail (Strings.cat [
                      "untyped let binding in ",
                      (Core__.term term)])) (\ts -> Flows.pure (Schemas.typeSchemeToFType ts)) (Core.bindingType b))
              in (Flows.bind (Flows.mapList btypeOf bs) (\btypes ->  
                let types2 = (Maps.union (Maps.fromList (Lists.zip bnames btypes)) types)
                in (Flows.bind (Flows.mapList (typeOfInternal cx vars types2 []) bterms) (\typeofs -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) btypes) (\_ -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) typeofs) (\_ -> Logic.ifElse (Equality.equal typeofs btypes) (typeOfInternal cx vars types2 [] body) (Flows.fail (Strings.cat [
                  "binding types disagree: ",
                  Formatting.showList Core__.type_ btypes,
                  " and ",
                  Formatting.showList Core__.type_ typeofs,
                  " from terms: ",
                  (Formatting.showList Core__.term bterms)]))))))))))
    Core.TermList v1 -> (Logic.ifElse (Lists.null v1) (Logic.ifElse (Equality.equal (Lists.length apptypes) 1) (Flows.pure (Core.TypeList (Lists.head apptypes))) (Flows.fail "list type applied to more or less than one argument")) (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) v1) (\eltypes -> Flows.bind (checkSameType "list elements" eltypes) (\unifiedType -> Flows.bind (checkTypeVariables cx vars unifiedType) (\_ -> Flows.pure (Core.TypeList unifiedType))))))
    Core.TermLiteral v1 -> (checkApp (Flows.pure (Core.TypeLiteral (Variants.literalType v1))))
    Core.TermMap v1 -> (Logic.ifElse (Maps.null v1) (Logic.ifElse (Equality.equal (Lists.length apptypes) 2) (Flows.pure (Core.TypeMap (Core.MapType {
      Core.mapTypeKeys = (Lists.at 1 apptypes),
      Core.mapTypeValues = (Lists.at 0 apptypes)}))) (Flows.fail "map type applied to more or less than two arguments")) (checkApp ( 
      let pairs = (Maps.toList v1)
      in (Flows.bind (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) (Lists.map fst pairs)) (checkSameType "map keys")) (\kt -> Flows.bind (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) (Lists.map snd pairs)) (checkSameType "map values")) (\vt -> Flows.bind (checkTypeVariables cx vars kt) (\_ -> Flows.bind (checkTypeVariables cx vars vt) (\_ -> Flows.pure (Core.TypeMap (Core.MapType {
        Core.mapTypeKeys = kt,
        Core.mapTypeValues = vt}))))))))))
    Core.TermOptional v1 -> (Optionals.maybe ( 
      let n = (Lists.length apptypes)
      in (Logic.ifElse (Equality.equal n 1) (Flows.pure (Core.TypeOptional (Lists.head apptypes))) (Flows.fail (Strings.cat [
        Strings.cat [
          "optional type applied to ",
          (Literals.showInt32 n)],
        " argument(s). Expected 1."])))) (\term -> checkApp (Flows.bind (typeOfInternal cx vars types [] term) (\termType -> Flows.bind (checkTypeVariables cx vars termType) (\_ -> Flows.pure (Core.TypeOptional termType))))) v1)
    Core.TermProduct v1 -> (checkApp (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) v1) (\etypes -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) etypes) (\_ -> Flows.pure (Core.TypeProduct etypes)))))
    Core.TermRecord v1 ->  
      let tname = (Core.recordTypeName v1)
      in  
        let fields = (Core.recordFields v1)
        in (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) (Lists.map Core.fieldTerm fields)) (\ftypes -> Flows.bind (Flows.mapList (checkTypeVariables cx vars) ftypes) (\_ -> typeOfNominal "record typeOf" cx tname (Core.TypeRecord (Core.RowType {
          Core.rowTypeTypeName = tname,
          Core.rowTypeFields = (Lists.zipWith (\n -> \t -> Core.FieldType {
            Core.fieldTypeName = n,
            Core.fieldTypeType = t}) (Lists.map Core.fieldName fields) ftypes)})))))
    Core.TermSet v1 -> (Logic.ifElse (Sets.null v1) (Logic.ifElse (Equality.equal (Lists.length apptypes) 1) (Flows.pure (Core.TypeSet (Lists.head apptypes))) (Flows.fail "set type applied to more or less than one argument")) (Flows.bind (Flows.mapList (typeOfInternal cx vars types []) (Sets.toList v1)) (\eltypes -> Flows.bind (checkSameType "set elements" eltypes) (\unifiedType -> Flows.bind (checkTypeVariables cx vars unifiedType) (\_ -> Flows.pure (Core.TypeSet unifiedType))))))
    Core.TermTypeLambda v1 ->  
      let v = (Core.typeLambdaParameter v1)
      in  
        let e = (Core.typeLambdaBody v1)
        in (Flows.bind (typeOfInternal cx (Sets.insert v vars) types [] e) (\t1 -> Flows.bind (checkTypeVariables cx (Sets.insert v vars) t1) (\_ -> Flows.pure (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = v,
          Core.forallTypeBody = t1})))))
    Core.TermTypeApplication v1 ->  
      let e = (Core.typedTermTerm v1)
      in  
        let t = (Core.typedTermType v1)
        in (typeOfInternal cx vars types (Lists.cons t apptypes) e)
    Core.TermUnion v1 ->  
      let tname = (Core.injectionTypeName v1)
      in  
        let field = (Core.injectionField v1)
        in  
          let fname = (Core.fieldName field)
          in  
            let fterm = (Core.fieldTerm field)
            in (Flows.bind (Schemas.requireSchemaType cx tname) (\schemaType ->  
              let svars = (Core.typeSchemeVariables schemaType)
              in  
                let stype = (Core.typeSchemeType schemaType)
                in (Flows.bind (Core_.unionType tname stype) (\sfields -> Flows.bind (Schemas.findFieldType fname sfields) (\ftyp -> Flows.pure (Schemas.nominalApplication tname apptypes))))))
    Core.TermUnit -> (checkApp (Flows.pure Core.TypeUnit))
    Core.TermVariable v1 -> (checkApp (Optionals.maybe (Flows.fail (Strings.cat [
      "unbound variable: ",
      (Core.unName v1)])) Flows.pure (Maps.lookup v1 types)))
    Core.TermWrap v1 ->  
      let tname = (Core.wrappedTermTypeName v1)
      in  
        let innerTerm = (Core.wrappedTermObject v1)
        in (Flows.bind (typeOfInternal cx vars types [] innerTerm) (\innerType -> Flows.bind (checkTypeVariables cx vars innerType) (\_ -> typeOfNominal "wrapper typeOf" cx tname (Core.TypeWrap (Core.WrappedType {
          Core.wrappedTypeTypeName = tname,
          Core.wrappedTypeObject = innerType})))))
    _ -> (Flows.fail (Strings.cat [
      "unsupported term variant in typeOf: ",
      (Mantle.termVariant (Variants.termVariant term))]))) term))

typeOfNominal :: (String -> Typing.InferenceContext -> Core.Name -> Core.Type -> Compute.Flow t0 Core.Type)
typeOfNominal desc cx tname expected =  
  let resolveType = (\subst -> \v -> Optionals.fromMaybe (Core.TypeVariable v) (Maps.lookup v subst))
  in (Flows.bind (Schemas.requireSchemaType cx tname) (\schemaType ->  
    let svars = (Core.typeSchemeVariables schemaType)
    in  
      let stype = (Core.typeSchemeType schemaType)
      in (Flows.bind (Unification.unifyTypes (Typing.inferenceContextSchemaTypes cx) stype expected desc) (\substWrapper -> Flows.bind (checkTypeSubst cx substWrapper) (\_ ->  
        let subst = (Typing.unTypeSubst substWrapper)
        in  
          let tparams = (Lists.map (resolveType subst) svars)
          in (Flows.pure (Schemas.nominalApplication tname tparams)))))))
