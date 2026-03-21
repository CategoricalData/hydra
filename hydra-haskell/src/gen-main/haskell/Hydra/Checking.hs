-- Note: this is an automatically generated file. Do not edit.

-- | Type checking and type reconstruction (type-of) for the results of Hydra unification and inference

module Hydra.Checking where

import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as Core__
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Graph as Graph
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Reflect as Reflect
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core___
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

allEqual :: Eq t0 => ([t0] -> Bool)
allEqual els =
    Logic.ifElse (Lists.null els) True (Lists.foldl (\b -> \t -> Logic.and b (Equality.equal t (Lists.head els))) True (Lists.tail els))

-- | Apply type arguments to a type, substituting forall-bound variables
applyTypeArgumentsToType :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Type -> Either (Context.InContext Errors.Error) Core.Type
applyTypeArgumentsToType cx tx typeArgs t =
    Logic.ifElse (Lists.null typeArgs) (Right t) (
      let nonnull =
              case t of
                Core.TypeForall v0 ->
                  let v = Core.forallTypeParameter v0
                      tbody = Core.forallTypeBody v0
                  in (applyTypeArgumentsToType cx tx (Lists.tail typeArgs) (Substitution.substInType (Typing.TypeSubst (Maps.singleton v (Lists.head typeArgs))) tbody))
                _ -> Left (Context.InContext {
                  Context.inContextObject = (Errors.ErrorOther (Errors.OtherError (Strings.cat [
                    "not a forall type: ",
                    (Core___.type_ t),
                    ". Trying to apply ",
                    (Literals.showInt32 (Lists.length typeArgs)),
                    " type args: ",
                    (Formatting.showList Core___.type_ typeArgs),
                    ". Context has vars: {",
                    (Strings.intercalate ", " (Lists.map Core.unName (Maps.keys (Graph.graphBoundTypes tx)))),
                    "}"]))),
                  Context.inContextContext = cx})
      in nonnull)

-- | Check that a term has no unbound type variables (Either version)
checkForUnboundTypeVariables :: Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Errors.Error) ()
checkForUnboundTypeVariables cx tx term0 =

      let svars = Sets.fromList (Maps.keys (Graph.graphSchemaTypes tx))
          checkRecursive =
                  \vars -> \trace -> \lbinding -> \term ->
                    let recurse = checkRecursive vars trace lbinding
                        dflt = Eithers.bind (Eithers.mapList recurse (Rewriting.subterms term)) (\_ -> Right ())
                        check =
                                \typ ->
                                  let freevars = Rewriting.freeVariablesInType typ
                                      badvars = Sets.difference (Sets.difference freevars vars) svars
                                  in (Logic.ifElse (Sets.null badvars) (Right ()) (Left (Context.InContext {
                                    Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorUnboundTypeVariables (Checking.UnboundTypeVariablesError {
                                      Checking.unboundTypeVariablesErrorVariables = badvars,
                                      Checking.unboundTypeVariablesErrorType = typ}))),
                                    Context.inContextContext = cx})))
                        checkOptional = \m -> Eithers.bind (Eithers.mapMaybe check m) (\_ -> Right ())
                    in case term of
                      Core.TermFunction v0 -> case v0 of
                        Core.FunctionElimination _ -> dflt
                        Core.FunctionLambda v1 -> Eithers.bind (checkOptional (Core.lambdaDomain v1)) (\_ -> recurse (Core.lambdaBody v1))
                        _ -> dflt
                      Core.TermLet v0 ->
                        let forBinding =
                                \b ->
                                  let bterm = Core.bindingTerm b
                                      newVars = Maybes.maybe vars (\ts -> Sets.union vars (Sets.fromList (Core.typeSchemeVariables ts))) (Core.bindingType b)
                                      newTrace = Lists.cons (Core.unName (Core.bindingName b)) trace
                                  in (checkRecursive newVars newTrace (Just b) bterm)
                        in (Eithers.bind (Eithers.mapList forBinding (Core.letBindings v0)) (\_ -> recurse (Core.letBody v0)))
                      Core.TermTypeApplication v0 -> Eithers.bind (check (Core.typeApplicationTermType v0)) (\_ -> recurse (Core.typeApplicationTermBody v0))
                      Core.TermTypeLambda v0 -> Eithers.bind (check (Core.TypeVariable (Core.typeLambdaParameter v0))) (\_ -> recurse (Core.typeLambdaBody v0))
                      _ -> dflt
      in (checkRecursive Sets.empty [
        "top level"] Nothing term0)

-- | Check that a nominal type is applied to the correct number of type arguments (Either version)
checkNominalApplication :: Context.Context -> Graph.Graph -> Core.Name -> [Core.Type] -> Either (Context.InContext Errors.Error) ((), Context.Context)
checkNominalApplication cx tx tname typeArgs =
    Eithers.bind (Schemas.requireSchemaType cx (Graph.graphSchemaTypes tx) tname) (\result ->
      let schemaType = Pairs.first result
          cx2 = Pairs.second result
          vars = Core.typeSchemeVariables schemaType
          varslen = Lists.length vars
          argslen = Lists.length typeArgs
      in (Logic.ifElse (Equality.equal varslen argslen) (Right ((), cx2)) (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorTypeArityMismatch (Checking.TypeArityMismatchError {
          Checking.typeArityMismatchErrorType = (Core.TypeVariable tname),
          Checking.typeArityMismatchErrorExpectedArity = varslen,
          Checking.typeArityMismatchErrorActualArity = argslen,
          Checking.typeArityMismatchErrorTypeArguments = typeArgs}))),
        Context.inContextContext = cx2}))))

-- | Ensure all types in a list are equal and return the common type
checkSameType :: Context.Context -> Graph.Graph -> String -> [Core.Type] -> Either (Context.InContext Errors.Error) Core.Type
checkSameType cx tx desc types =
    Logic.ifElse (typesAllEffectivelyEqual tx types) (Right (Lists.head types)) (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorUnequalTypes (Checking.UnequalTypesError {
        Checking.unequalTypesErrorTypes = types,
        Checking.unequalTypesErrorDescription = desc}))),
      Context.inContextContext = cx}))

-- | Check that a term has the expected type
checkType :: Context.Context -> Graph.Graph -> Core.Term -> Core.Type -> Either (Context.InContext Errors.Error) ()
checkType cx tx term typ =

      let vars = Graph.graphTypeVariables tx
      in (Logic.ifElse Constants.debugInference (Eithers.bind (Eithers.map (\_p -> Pairs.first _p) (typeOf cx tx [] term)) (\t0 -> Logic.ifElse (typesEffectivelyEqual tx t0 typ) (Right ()) (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorTypeMismatch (Checking.TypeMismatchError {
          Checking.typeMismatchErrorExpectedType = typ,
          Checking.typeMismatchErrorActualType = t0}))),
        Context.inContextContext = cx})))) (Right ()))

-- | Sanity-check a type substitution arising from unification. Specifically, check that schema types have not been inappropriately unified with type variables inferred from terms.
checkTypeSubst :: Context.Context -> Graph.Graph -> Typing.TypeSubst -> Either (Context.InContext Errors.Error) Typing.TypeSubst
checkTypeSubst cx tx subst =

      let s = Typing.unTypeSubst subst
          vars = Sets.fromList (Maps.keys s)
          suspectVars = Sets.intersection vars (Sets.fromList (Maps.keys (Graph.graphSchemaTypes tx)))
          isNominal =
                  \ts -> case (Rewriting.deannotateType (Core.typeSchemeType ts)) of
                    Core.TypeRecord _ -> True
                    Core.TypeUnion _ -> True
                    Core.TypeWrap _ -> True
                    _ -> False
          badVars =
                  Sets.fromList (Lists.filter (\v -> Maybes.maybe False isNominal (Lexical.dereferenceSchemaType v (Graph.graphSchemaTypes tx))) (Sets.toList suspectVars))
          badPairs = Lists.filter (\p -> Sets.member (Pairs.first p) badVars) (Maps.toList s)
          printPair = \p -> Strings.cat2 (Strings.cat2 (Core.unName (Pairs.first p)) " --> ") (Core___.type_ (Pairs.second p))
      in (Logic.ifElse (Sets.null badVars) (Right subst) (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorIncorrectUnification (Checking.IncorrectUnificationError {
          Checking.incorrectUnificationErrorSubstitution = subst}))),
        Context.inContextContext = cx})))

-- | Check that all type variables in a type are bound. NOTE: This check is currently disabled to allow phantom type variables from polymorphic instantiation to pass through. The proper fix is to ensure `typeOf` doesn't create fresh variables for post-inference code.
checkTypeVariables :: t0 -> t1 -> ()
checkTypeVariables _tx _typ = ()

-- | Check if a type contains any type variable from the current scope
containsInScopeTypeVars :: Graph.Graph -> Core.Type -> Bool
containsInScopeTypeVars tx t =

      let vars = Graph.graphTypeVariables tx
          freeVars = Rewriting.freeVariablesInTypeSimple t
      in (Logic.not (Sets.null (Sets.intersection vars freeVars)))

-- | Normalize free type variables in a type to canonical names based on order of first occurrence. This allows comparing types that differ only in the naming of free type variables.
normalizeTypeFreeVars :: Core.Type -> Core.Type
normalizeTypeFreeVars typ =

      let collectVars =
              \acc -> \t -> case t of
                Core.TypeVariable v0 -> Logic.ifElse (Maps.member v0 acc) acc (Maps.insert v0 (Core.Name (Strings.cat2 "_tv" (Literals.showInt32 (Maps.size acc)))) acc)
                _ -> acc
          subst = Rewriting.foldOverType Coders.TraversalOrderPre collectVars Maps.empty typ
      in (Rewriting.substituteTypeVariables subst typ)

-- | Get the bound types from a graph as a type environment
toFContext :: Graph.Graph -> M.Map Core.Name Core.Type
toFContext cx = Maps.map Rewriting.typeSchemeToFType (Graph.graphBoundTypes cx)

-- | Check whether two lists of types are effectively equal, disregarding type aliases
typeListsEffectivelyEqual :: Graph.Graph -> [Core.Type] -> [Core.Type] -> Bool
typeListsEffectivelyEqual tx tlist1 tlist2 =
    Logic.ifElse (Equality.equal (Lists.length tlist1) (Lists.length tlist2)) (Lists.foldl Logic.and True (Lists.zipWith (typesEffectivelyEqual tx) tlist1 tlist2)) False

-- | Given a type context, reconstruct the type of a System F term
typeOf :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Term -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOf cx tx typeArgs term =

      let cx1 =
              Context.Context {
                Context.contextTrace = (Lists.cons "typeOf" (Context.contextTrace cx)),
                Context.contextMessages = (Context.contextMessages cx),
                Context.contextOther = (Context.contextOther cx)}
      in case term of
        Core.TermAnnotated v0 -> typeOfAnnotatedTerm cx1 tx typeArgs v0
        Core.TermApplication v0 -> typeOfApplication cx1 tx typeArgs v0
        Core.TermEither v0 -> typeOfEither cx1 tx typeArgs v0
        Core.TermFunction v0 -> case v0 of
          Core.FunctionElimination v1 -> case v1 of
            Core.EliminationRecord v2 -> typeOfProjection cx1 tx typeArgs v2
            Core.EliminationUnion v2 -> typeOfCaseStatement cx1 tx typeArgs v2
            Core.EliminationWrap v2 -> typeOfUnwrap cx1 tx typeArgs v2
          Core.FunctionLambda v1 -> typeOfLambda cx1 tx typeArgs v1
          Core.FunctionPrimitive v1 -> typeOfPrimitive cx1 tx typeArgs v1
        Core.TermLet v0 -> typeOfLet cx1 tx typeArgs v0
        Core.TermList v0 -> typeOfList cx1 tx typeArgs v0
        Core.TermLiteral v0 -> typeOfLiteral cx1 tx typeArgs v0
        Core.TermMap v0 -> typeOfMap cx1 tx typeArgs v0
        Core.TermMaybe v0 -> typeOfMaybe cx1 tx typeArgs v0
        Core.TermPair v0 -> typeOfPair cx1 tx typeArgs v0
        Core.TermRecord v0 -> typeOfRecord cx1 tx typeArgs v0
        Core.TermSet v0 -> typeOfSet cx1 tx typeArgs v0
        Core.TermTypeApplication v0 -> typeOfTypeApplication cx1 tx typeArgs v0
        Core.TermTypeLambda v0 -> typeOfTypeLambda cx1 tx typeArgs v0
        Core.TermUnion v0 -> typeOfInjection cx1 tx typeArgs v0
        Core.TermUnit -> typeOfUnit cx1 tx typeArgs
        Core.TermVariable v0 -> typeOfVariable cx1 tx typeArgs v0
        Core.TermWrap v0 -> typeOfWrappedTerm cx1 tx typeArgs v0
        _ -> Left (Context.InContext {
          Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorUnsupportedTermVariant (Checking.UnsupportedTermVariantError {
            Checking.unsupportedTermVariantErrorTermVariant = (Reflect.termVariant term)}))),
          Context.inContextContext = cx1})

-- | Reconstruct the type of an annotated term (Either/Context version)
typeOfAnnotatedTerm :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.AnnotatedTerm -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfAnnotatedTerm cx tx typeArgs at = typeOf cx tx typeArgs (Core.annotatedTermBody at)

-- | Reconstruct the type of an application term (Either/Context version)
typeOfApplication :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Application -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfApplication cx tx typeArgs app =

      let fun = Core.applicationFunction app
          arg = Core.applicationArgument app
          tryType =
                  \cx0 -> \tfun -> \targ -> case tfun of
                    Core.TypeForall v0 -> tryType cx0 (Core.forallTypeBody v0) targ
                    Core.TypeFunction v0 ->
                      let dom = Core.functionTypeDomain v0
                          cod = Core.functionTypeCodomain v0
                      in (Logic.ifElse (typesEffectivelyEqual tx dom targ) (Right (cod, cx0)) (Left (Context.InContext {
                        Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorTypeMismatch (Checking.TypeMismatchError {
                          Checking.typeMismatchErrorExpectedType = dom,
                          Checking.typeMismatchErrorActualType = targ}))),
                        Context.inContextContext = cx0})))
                    Core.TypeVariable _ ->
                      let nameResult = Schemas.freshName cx0
                          freshN = Pairs.first nameResult
                          cx1 = Pairs.second nameResult
                      in (Right (Core.TypeVariable freshN, cx1))
                    _ -> Left (Context.InContext {
                      Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorNotAFunctionType (Checking.NotAFunctionTypeError {
                        Checking.notAFunctionTypeErrorType = tfun}))),
                      Context.inContextContext = cx0})
      in (Eithers.bind (typeOf cx tx [] fun) (\result1 ->
        let tfun = Pairs.first result1
            cx2 = Pairs.second result1
        in (Eithers.bind (typeOf cx2 tx [] arg) (\result2 ->
          let targ = Pairs.first result2
              cx3 = Pairs.second result2
          in (Eithers.bind (tryType cx3 tfun targ) (\result3 ->
            let t = Pairs.first result3
                cx4 = Pairs.second result3
            in (Eithers.bind (applyTypeArgumentsToType cx4 tx typeArgs t) (\applied -> Right (applied, cx4)))))))))

-- | Reconstruct the type of a case statement (Either/Context version)
typeOfCaseStatement :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.CaseStatement -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfCaseStatement cx tx typeArgs cs =

      let tname = Core.caseStatementTypeName cs
          dflt = Core.caseStatementDefault cs
          cases = Core.caseStatementCases cs
          cterms = Lists.map Core.fieldTerm cases
      in (Eithers.bind (Eithers.mapMaybe (\e -> typeOf cx tx [] e) dflt) (\dfltResult ->
        let tdflt = Maybes.map Pairs.first dfltResult
            cx2 = Maybes.maybe cx Pairs.second dfltResult
            foldResult =
                    Lists.foldl (\acc -> \term -> Eithers.bind acc (\accR ->
                      let types = Pairs.first accR
                          cxA = Pairs.second accR
                      in (Eithers.bind (typeOf cxA tx [] term) (\tResult ->
                        let t = Pairs.first tResult
                            cxB = Pairs.second tResult
                        in (Right (Lists.concat2 types (Lists.pure t), cxB)))))) (Right ([], cx2)) cterms
        in (Eithers.bind foldResult (\foldR ->
          let tcterms = Pairs.first foldR
              cx3 = Pairs.second foldR
              fcodsResult =
                      Lists.foldl (\acc -> \t -> Eithers.bind acc (\accR ->
                        let cods = Pairs.first accR
                        in (Eithers.bind (Core__.functionType cx3 t) (\ft -> Right (Lists.concat2 cods (Lists.pure (Core.functionTypeCodomain ft)), cx3))))) (Right ([], cx3)) tcterms
          in (Eithers.bind fcodsResult (\fcodsR ->
            let fcods = Pairs.first fcodsR
                cods = Maybes.cat (Lists.cons tdflt (Lists.map Maybes.pure fcods))
            in (Eithers.bind (checkSameType cx3 tx "case branches" cods) (\cod -> Right (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Schemas.nominalApplication tname typeArgs),
              Core.functionTypeCodomain = cod}), cx3)))))))))

-- | Reconstruct the type of an either value (Either/Context version)
typeOfEither :: Context.Context -> Graph.Graph -> [Core.Type] -> Either Core.Term Core.Term -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfEither cx tx typeArgs et =

      let n = Lists.length typeArgs
      in (Logic.ifElse (Equality.equal n 2) (Eithers.either (\leftTerm -> Eithers.bind (typeOf cx tx [] leftTerm) (\result ->
        let leftType = Pairs.first result
            cx2 = Pairs.second result
        in (Right (Core.TypeEither (Core.EitherType {
          Core.eitherTypeLeft = leftType,
          Core.eitherTypeRight = (Lists.at 1 typeArgs)}), cx2)))) (\rightTerm -> Eithers.bind (typeOf cx tx [] rightTerm) (\result ->
        let rightType = Pairs.first result
            cx2 = Pairs.second result
        in (Right (Core.TypeEither (Core.EitherType {
          Core.eitherTypeLeft = (Lists.at 0 typeArgs),
          Core.eitherTypeRight = rightType}), cx2)))) et) (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorTypeArityMismatch (Checking.TypeArityMismatchError {
          Checking.typeArityMismatchErrorType = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = Core.TypeUnit,
            Core.eitherTypeRight = Core.TypeUnit})),
          Checking.typeArityMismatchErrorExpectedArity = 2,
          Checking.typeArityMismatchErrorActualArity = n,
          Checking.typeArityMismatchErrorTypeArguments = typeArgs}))),
        Context.inContextContext = cx})))

-- | Reconstruct the type of a union injection (Either/Context version)
typeOfInjection :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Injection -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfInjection cx tx typeArgs injection =

      let tname = Core.injectionTypeName injection
          field = Core.injectionField injection
          fname = Core.fieldName field
          fterm = Core.fieldTerm field
      in (Eithers.bind (Schemas.requireSchemaType cx (Graph.graphSchemaTypes tx) tname) (\schemaResult ->
        let schemaType = Pairs.first schemaResult
            cx2 = Pairs.second schemaResult
            svars = Core.typeSchemeVariables schemaType
            sbody = Core.typeSchemeType schemaType
        in (Eithers.bind (Core__.unionType cx2 tname sbody) (\sfields -> Eithers.bind (Schemas.findFieldType cx2 fname sfields) (\ftyp -> Right (Schemas.nominalApplication tname typeArgs, cx2))))))

-- | Reconstruct the type of a lambda function (Either/Context version)
typeOfLambda :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Lambda -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfLambda cx tx typeArgs l =

      let v = Core.lambdaParameter l
          mdom = Core.lambdaDomain l
          body = Core.lambdaBody l
      in (Eithers.bind (Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorUntypedLambda (Checking.UntypedLambdaError {
        }))),
        Context.inContextContext = cx})) (\dom ->
        let types2 = Maps.insert v (Rewriting.fTypeToTypeScheme dom) (Graph.graphBoundTypes tx)
        in (Eithers.bind (typeOf cx (Graph.Graph {
          Graph.graphBoundTerms = (Graph.graphBoundTerms tx),
          Graph.graphBoundTypes = types2,
          Graph.graphClassConstraints = (Graph.graphClassConstraints tx),
          Graph.graphLambdaVariables = (Graph.graphLambdaVariables tx),
          Graph.graphMetadata = (Graph.graphMetadata tx),
          Graph.graphPrimitives = (Graph.graphPrimitives tx),
          Graph.graphSchemaTypes = (Graph.graphSchemaTypes tx),
          Graph.graphTypeVariables = (Graph.graphTypeVariables tx)}) [] body) (\codResult ->
          let cod = Pairs.first codResult
              cx2 = Pairs.second codResult
          in (Right (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = dom,
            Core.functionTypeCodomain = cod}), cx2))))) mdom) (\tbodyResult ->
        let tbody = Pairs.first tbodyResult
            cx3 = Pairs.second tbodyResult
        in (Eithers.bind (applyTypeArgumentsToType cx3 tx typeArgs tbody) (\applied -> Right (applied, cx3)))))

-- | Reconstruct the type of a let binding (Either/Context version)
typeOfLet :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Let -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfLet cx tx typeArgs letTerm =

      let bs = Core.letBindings letTerm
          body = Core.letBody letTerm
          bnames = Lists.map Core.bindingName bs
          bindingType =
                  \b -> Maybes.maybe (Left (Context.InContext {
                    Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorUntypedLetBinding (Checking.UntypedLetBindingError {
                      Checking.untypedLetBindingErrorBinding = b}))),
                    Context.inContextContext = cx})) (\ts -> Right (Rewriting.typeSchemeToFType ts)) (Core.bindingType b)
          btypesResult =
                  Lists.foldl (\acc -> \b -> Eithers.bind acc (\accR ->
                    let types = Pairs.first accR
                    in (Eithers.bind (bindingType b) (\btype -> Right (Lists.concat2 types (Lists.pure btype), ()))))) (Right ([], ())) bs
      in (Eithers.bind btypesResult (\btypesR ->
        let btypes = Pairs.first btypesR
            tx2 =
                    Graph.Graph {
                      Graph.graphBoundTerms = (Graph.graphBoundTerms tx),
                      Graph.graphBoundTypes = (Maps.union (Maps.fromList (Lists.zip bnames (Lists.map Rewriting.fTypeToTypeScheme btypes))) (Graph.graphBoundTypes tx)),
                      Graph.graphClassConstraints = (Graph.graphClassConstraints tx),
                      Graph.graphLambdaVariables = (Graph.graphLambdaVariables tx),
                      Graph.graphMetadata = (Graph.graphMetadata tx),
                      Graph.graphPrimitives = (Graph.graphPrimitives tx),
                      Graph.graphSchemaTypes = (Graph.graphSchemaTypes tx),
                      Graph.graphTypeVariables = (Graph.graphTypeVariables tx)}
        in (Eithers.bind (typeOf cx tx2 [] body) (\tResult ->
          let t = Pairs.first tResult
              cx2 = Pairs.second tResult
          in (Eithers.bind (applyTypeArgumentsToType cx2 tx typeArgs t) (\applied -> Right (applied, cx2)))))))

-- | Reconstruct the type of a list (Either/Context version)
typeOfList :: Context.Context -> Graph.Graph -> [Core.Type] -> [Core.Term] -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfList cx tx typeArgs els =
    Logic.ifElse (Lists.null els) (Logic.ifElse (Equality.equal (Lists.length typeArgs) 1) (Right (Core.TypeList (Lists.head typeArgs), cx)) (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorTypeArityMismatch (Checking.TypeArityMismatchError {
        Checking.typeArityMismatchErrorType = (Core.TypeList Core.TypeUnit),
        Checking.typeArityMismatchErrorExpectedArity = 1,
        Checking.typeArityMismatchErrorActualArity = (Lists.length typeArgs),
        Checking.typeArityMismatchErrorTypeArguments = typeArgs}))),
      Context.inContextContext = cx}))) (
      let foldResult =
              Lists.foldl (\acc -> \term -> Eithers.bind acc (\accR ->
                let types = Pairs.first accR
                    cxA = Pairs.second accR
                in (Eithers.bind (typeOf cxA tx [] term) (\tResult ->
                  let t = Pairs.first tResult
                      cxB = Pairs.second tResult
                  in (Right (Lists.concat2 types (Lists.pure t), cxB)))))) (Right ([], cx)) els
      in (Eithers.bind foldResult (\foldR ->
        let eltypes = Pairs.first foldR
            cx2 = Pairs.second foldR
        in (Eithers.bind (checkSameType cx2 tx "list elements" eltypes) (\unifiedType -> Right (Core.TypeList unifiedType, cx2))))))

-- | Reconstruct the type of a literal (Either/Context version)
typeOfLiteral :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Literal -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfLiteral cx tx typeArgs lit =

      let t = Core.TypeLiteral (Reflect.literalType lit)
      in (Eithers.bind (applyTypeArgumentsToType cx tx typeArgs t) (\applied -> Right (applied, cx)))

-- | Reconstruct the type of a map (Either/Context version)
typeOfMap :: Context.Context -> Graph.Graph -> [Core.Type] -> M.Map Core.Term Core.Term -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfMap cx tx typeArgs m =
    Logic.ifElse (Maps.null m) (Logic.ifElse (Equality.equal (Lists.length typeArgs) 2) (Right (Core.TypeMap (Core.MapType {
      Core.mapTypeKeys = (Lists.at 0 typeArgs),
      Core.mapTypeValues = (Lists.at 1 typeArgs)}), cx)) (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorTypeArityMismatch (Checking.TypeArityMismatchError {
        Checking.typeArityMismatchErrorType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = Core.TypeUnit,
          Core.mapTypeValues = Core.TypeUnit})),
        Checking.typeArityMismatchErrorExpectedArity = 2,
        Checking.typeArityMismatchErrorActualArity = (Lists.length typeArgs),
        Checking.typeArityMismatchErrorTypeArguments = typeArgs}))),
      Context.inContextContext = cx}))) (
      let pairs = Maps.toList m
          keyFoldResult =
                  Lists.foldl (\acc -> \p -> Eithers.bind acc (\accR ->
                    let types = Pairs.first accR
                        cxA = Pairs.second accR
                    in (Eithers.bind (typeOf cxA tx [] (Pairs.first p)) (\tResult ->
                      let t = Pairs.first tResult
                          cxB = Pairs.second tResult
                      in (Right (Lists.concat2 types (Lists.pure t), cxB)))))) (Right ([], cx)) pairs
      in (Eithers.bind keyFoldResult (\keyFoldR ->
        let keyTypes = Pairs.first keyFoldR
            cx2 = Pairs.second keyFoldR
        in (Eithers.bind (checkSameType cx2 tx "map keys" keyTypes) (\kt ->
          let valFoldResult =
                  Lists.foldl (\acc -> \p -> Eithers.bind acc (\accR ->
                    let types = Pairs.first accR
                        cxA = Pairs.second accR
                    in (Eithers.bind (typeOf cxA tx [] (Pairs.second p)) (\tResult ->
                      let t = Pairs.first tResult
                          cxB = Pairs.second tResult
                      in (Right (Lists.concat2 types (Lists.pure t), cxB)))))) (Right ([], cx2)) pairs
          in (Eithers.bind valFoldResult (\valFoldR ->
            let valTypes = Pairs.first valFoldR
                cx3 = Pairs.second valFoldR
            in (Eithers.bind (checkSameType cx3 tx "map values" valTypes) (\vt -> Eithers.bind (applyTypeArgumentsToType cx3 tx typeArgs (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = kt,
              Core.mapTypeValues = vt}))) (\applied -> Right (applied, cx3)))))))))))

-- | Reconstruct the type of an optional value (Either/Context version)
typeOfMaybe :: Context.Context -> Graph.Graph -> [Core.Type] -> Maybe Core.Term -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfMaybe cx tx typeArgs mt =

      let forNothing =

                let n = Lists.length typeArgs
                in (Logic.ifElse (Equality.equal n 1) (Right (Core.TypeMaybe (Lists.head typeArgs), cx)) (Left (Context.InContext {
                  Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorTypeArityMismatch (Checking.TypeArityMismatchError {
                    Checking.typeArityMismatchErrorType = (Core.TypeMaybe Core.TypeUnit),
                    Checking.typeArityMismatchErrorExpectedArity = 1,
                    Checking.typeArityMismatchErrorActualArity = n,
                    Checking.typeArityMismatchErrorTypeArguments = typeArgs}))),
                  Context.inContextContext = cx})))
          forJust =
                  \term -> Eithers.bind (typeOf cx tx [] term) (\tResult ->
                    let termType = Pairs.first tResult
                        cx2 = Pairs.second tResult
                        t = Core.TypeMaybe termType
                    in (Eithers.bind (applyTypeArgumentsToType cx2 tx typeArgs t) (\applied -> Right (applied, cx2))))
      in (Maybes.maybe forNothing forJust mt)

-- | Reconstruct the type of a pair (Either/Context version)
typeOfPair :: Context.Context -> Graph.Graph -> [Core.Type] -> (Core.Term, Core.Term) -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfPair cx tx typeArgs p =

      let n = Lists.length typeArgs
      in (Logic.ifElse (Equality.equal n 2) (
        let pairFst = Pairs.first p
            pairSnd = Pairs.second p
        in (Eithers.bind (typeOf cx tx [] pairFst) (\result1 ->
          let firstType = Pairs.first result1
              cx2 = Pairs.second result1
          in (Eithers.bind (typeOf cx2 tx [] pairSnd) (\result2 ->
            let secondType = Pairs.first result2
                cx3 = Pairs.second result2
            in (Right (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = firstType,
              Core.pairTypeSecond = secondType}), cx3))))))) (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorTypeArityMismatch (Checking.TypeArityMismatchError {
          Checking.typeArityMismatchErrorType = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = Core.TypeUnit,
            Core.pairTypeSecond = Core.TypeUnit})),
          Checking.typeArityMismatchErrorExpectedArity = 2,
          Checking.typeArityMismatchErrorActualArity = n,
          Checking.typeArityMismatchErrorTypeArguments = typeArgs}))),
        Context.inContextContext = cx})))

-- | Reconstruct the type of a primitive function (Either/Context version)
typeOfPrimitive :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Name -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfPrimitive cx tx typeArgs name =

      let rawTs = Maybes.map (\_p -> Graph.primitiveType _p) (Maps.lookup name (Graph.graphPrimitives tx))
      in (Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorUndefinedTerm (Core_.UndefinedTermError {
          Core_.undefinedTermErrorName = name})),
        Context.inContextContext = cx})) (\tsRaw ->
        let instResult = Schemas.instantiateTypeScheme cx tsRaw
            ts = Pairs.first instResult
            cx2 = Pairs.second instResult
            t = Rewriting.typeSchemeToFType ts
        in (Eithers.bind (applyTypeArgumentsToType cx2 tx typeArgs t) (\applied -> Right (applied, cx2)))) rawTs)

-- | Reconstruct the type of a record projection (Either/Context version)
typeOfProjection :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Projection -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfProjection cx tx typeArgs p =

      let tname = Core.projectionTypeName p
          fname = Core.projectionField p
      in (Eithers.bind (Schemas.requireSchemaType cx (Graph.graphSchemaTypes tx) tname) (\schemaResult ->
        let schemaType = Pairs.first schemaResult
            cx2 = Pairs.second schemaResult
            svars = Core.typeSchemeVariables schemaType
            sbody = Core.typeSchemeType schemaType
        in (Eithers.bind (Core__.recordType cx2 tname sbody) (\sfields -> Eithers.bind (Schemas.findFieldType cx2 fname sfields) (\ftyp ->
          let subst = Typing.TypeSubst (Maps.fromList (Lists.zip svars typeArgs))
              sftyp = Substitution.substInType subst ftyp
          in (Right (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Schemas.nominalApplication tname typeArgs),
            Core.functionTypeCodomain = sftyp}), cx2)))))))

-- | Reconstruct the type of a record (Either/Context version)
typeOfRecord :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Record -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfRecord cx tx typeArgs record =

      let tname = Core.recordTypeName record
          fields = Core.recordFields record
          foldResult =
                  Lists.foldl (\acc -> \term -> Eithers.bind acc (\accR ->
                    let types = Pairs.first accR
                        cxA = Pairs.second accR
                    in (Eithers.bind (typeOf cxA tx [] term) (\tResult ->
                      let t = Pairs.first tResult
                          cxB = Pairs.second tResult
                      in (Right (Lists.concat2 types (Lists.pure t), cxB)))))) (Right ([], cx)) (Lists.map Core.fieldTerm fields)
      in (Eithers.bind foldResult (\foldR ->
        let cx2 = Pairs.second foldR
        in (Right (Schemas.nominalApplication tname typeArgs, cx2))))

-- | Reconstruct the type of a set (Either/Context version)
typeOfSet :: Context.Context -> Graph.Graph -> [Core.Type] -> S.Set Core.Term -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfSet cx tx typeArgs els =
    Logic.ifElse (Sets.null els) (Logic.ifElse (Equality.equal (Lists.length typeArgs) 1) (Right (Core.TypeSet (Lists.head typeArgs), cx)) (Left (Context.InContext {
      Context.inContextObject = (Errors.ErrorChecking (Checking.CheckingErrorTypeArityMismatch (Checking.TypeArityMismatchError {
        Checking.typeArityMismatchErrorType = (Core.TypeSet Core.TypeUnit),
        Checking.typeArityMismatchErrorExpectedArity = 1,
        Checking.typeArityMismatchErrorActualArity = (Lists.length typeArgs),
        Checking.typeArityMismatchErrorTypeArguments = typeArgs}))),
      Context.inContextContext = cx}))) (
      let foldResult =
              Lists.foldl (\acc -> \term -> Eithers.bind acc (\accR ->
                let types = Pairs.first accR
                    cxA = Pairs.second accR
                in (Eithers.bind (typeOf cxA tx [] term) (\tResult ->
                  let t = Pairs.first tResult
                      cxB = Pairs.second tResult
                  in (Right (Lists.concat2 types (Lists.pure t), cxB)))))) (Right ([], cx)) (Sets.toList els)
      in (Eithers.bind foldResult (\foldR ->
        let eltypes = Pairs.first foldR
            cx2 = Pairs.second foldR
        in (Eithers.bind (checkSameType cx2 tx "set elements" eltypes) (\unifiedType -> Right (Core.TypeSet unifiedType, cx2))))))

-- | Reconstruct the type of a type application term (Either/Context version)
typeOfTypeApplication :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.TypeApplicationTerm -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfTypeApplication cx tx typeArgs tyapp =

      let body = Core.typeApplicationTermBody tyapp
          t = Core.typeApplicationTermType tyapp
      in (typeOf cx tx (Lists.cons t typeArgs) body)

-- | Reconstruct the type of a type lambda (type abstraction) term (Either/Context version)
typeOfTypeLambda :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.TypeLambda -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfTypeLambda cx tx typeArgs tl =

      let v = Core.typeLambdaParameter tl
          body = Core.typeLambdaBody tl
          vars = Graph.graphTypeVariables tx
          tx2 =
                  Graph.Graph {
                    Graph.graphBoundTerms = (Graph.graphBoundTerms tx),
                    Graph.graphBoundTypes = (Graph.graphBoundTypes tx),
                    Graph.graphClassConstraints = (Graph.graphClassConstraints tx),
                    Graph.graphLambdaVariables = (Graph.graphLambdaVariables tx),
                    Graph.graphMetadata = (Graph.graphMetadata tx),
                    Graph.graphPrimitives = (Graph.graphPrimitives tx),
                    Graph.graphSchemaTypes = (Graph.graphSchemaTypes tx),
                    Graph.graphTypeVariables = (Sets.insert v vars)}
      in (Eithers.bind (typeOf cx tx2 [] body) (\result1 ->
        let t1 = Pairs.first result1
            cx2 = Pairs.second result1
        in (Eithers.bind (applyTypeArgumentsToType cx2 tx typeArgs (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = v,
          Core.forallTypeBody = t1}))) (\applied -> Right (applied, cx2)))))

-- | Reconstruct the type of the unit term (Either/Context version)
typeOfUnit :: Context.Context -> Graph.Graph -> [Core.Type] -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfUnit cx tx typeArgs =
    Eithers.bind (applyTypeArgumentsToType cx tx typeArgs Core.TypeUnit) (\applied -> Right (applied, cx))

-- | Reconstruct the type of an unwrap operation (Either/Context version)
typeOfUnwrap :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Name -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfUnwrap cx tx typeArgs tname =
    Eithers.bind (Schemas.requireSchemaType cx (Graph.graphSchemaTypes tx) tname) (\schemaResult ->
      let schemaType = Pairs.first schemaResult
          cx2 = Pairs.second schemaResult
          svars = Core.typeSchemeVariables schemaType
          sbody = Core.typeSchemeType schemaType
      in (Eithers.bind (Core__.wrappedType cx2 tname sbody) (\wrapped ->
        let subst = Typing.TypeSubst (Maps.fromList (Lists.zip svars typeArgs))
            swrapped = Substitution.substInType subst wrapped
        in (Right (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Schemas.nominalApplication tname typeArgs),
          Core.functionTypeCodomain = swrapped}), cx2)))))

-- | Reconstruct the type of a variable (Either/Context version)
typeOfVariable :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.Name -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfVariable cx tx typeArgs name =

      let rawTypeScheme = Maps.lookup name (Graph.graphBoundTypes tx)
      in (Maybes.maybe (Left (Context.InContext {
        Context.inContextObject = (Errors.ErrorUndefinedType (Core_.UndefinedTypeError {
          Core_.undefinedTypeErrorName = name})),
        Context.inContextContext = cx})) (\ts ->
        let tResult =
                Logic.ifElse (Lists.null typeArgs) (Schemas.instantiateType cx (Rewriting.typeSchemeToFType ts)) (Rewriting.typeSchemeToFType ts, cx)
            t = Pairs.first tResult
            cx2 = Pairs.second tResult
        in (Eithers.bind (applyTypeArgumentsToType cx2 tx typeArgs t) (\applied -> Right (applied, cx2)))) rawTypeScheme)

-- | Reconstruct the type of a wrapped term (Either/Context version)
typeOfWrappedTerm :: Context.Context -> Graph.Graph -> [Core.Type] -> Core.WrappedTerm -> Either (Context.InContext Errors.Error) (Core.Type, Context.Context)
typeOfWrappedTerm cx tx typeArgs wt =

      let tname = Core.wrappedTermTypeName wt
          body = Core.wrappedTermBody wt
      in (Eithers.bind (typeOf cx tx [] body) (\result ->
        let cx2 = Pairs.second result
        in (Right (Schemas.nominalApplication tname typeArgs, cx2))))

-- | Check whether a list of types are effectively equal, disregarding type aliases and free type variable naming. Also treats free type variables (not in schema) as wildcards, since inference has already verified consistency.
typesAllEffectivelyEqual :: Graph.Graph -> [Core.Type] -> Bool
typesAllEffectivelyEqual tx tlist =

      let types = Graph.graphSchemaTypes tx
          containsFreeVar =
                  \t ->
                    let allVars = Rewriting.freeVariablesInTypeSimple t
                        schemaNames = Sets.fromList (Maps.keys types)
                    in (Logic.not (Sets.null (Sets.difference allVars schemaNames)))
          anyContainsFreeVar = Lists.foldl (\acc -> \t -> Logic.or acc (containsFreeVar t)) False tlist
      in (Logic.ifElse anyContainsFreeVar True (Logic.ifElse (allEqual (Lists.map (\t -> normalizeTypeFreeVars t) tlist)) True (allEqual (Lists.map (\t -> normalizeTypeFreeVars (Rewriting.deannotateTypeRecursive (Rewriting.replaceTypedefs types t))) tlist))))

-- | Check whether two types are effectively equal, disregarding type aliases, forall quantifiers, and treating in-scope type variables as wildcards
typesEffectivelyEqual :: Graph.Graph -> Core.Type -> Core.Type -> Bool
typesEffectivelyEqual tx t1 t2 =
    Logic.or (containsInScopeTypeVars tx t1) (Logic.or (containsInScopeTypeVars tx t2) (typesAllEffectivelyEqual tx [
      Schemas.fullyStripAndNormalizeType t1,
      (Schemas.fullyStripAndNormalizeType t2)]))
