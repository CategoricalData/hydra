-- Note: this is an automatically generated file. Do not edit.

-- | Coq code generator: converts Hydra modules to Coq source

module Hydra.Coq.Coder where

import qualified Hydra.Coq.Environment as Environment
import qualified Hydra.Coq.Language as Language
import qualified Hydra.Coq.Syntax as Syntax
import qualified Hydra.Coq.Utils as Utils
import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
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
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Build the Coq dependent-function term `forall (_ : dom), cod` used as the arrow type
coqArrow :: Syntax.Term -> Syntax.Term -> Syntax.Term
coqArrow dom cod =
    Syntax.TermForallOrFun (Syntax.ForallOrFunForall (Syntax.Forall {
      Syntax.forallBinders = (Syntax.OpenBindersBinders [
        Syntax.BinderType (Syntax.TypeBinders {
          Syntax.typeBindersNames = [
            Syntax.Name Nothing],
          Syntax.typeBindersType = (coqTypeTerm dom)})]),
      Syntax.forallType = (coqTypeTerm cod)}))

coqIdent :: String -> Syntax.Ident
coqIdent s = Syntax.Ident (Syntax.String_ s)

coqName :: String -> Syntax.Name
coqName s = Syntax.Name (Just (coqIdent s))

coqQualid :: String -> Syntax.Qualid
coqQualid s =
    Syntax.Qualid {
      Syntax.qualidId = (coqIdent s),
      Syntax.qualidFieldIds = []}

-- | Apply a Coq term to a list of argument terms, parenthesising each
coqTermApp :: Syntax.Term -> [Syntax.Term] -> Syntax.Term
coqTermApp f args =
    Logic.ifElse (Lists.null args) f (Syntax.TermTerm100 (Syntax.Term100Term10 (Syntax.Term10Application (Syntax.ApplicationNormal (Syntax.NormalApplication {
      Syntax.normalApplicationLhs = (Syntax.Term1Term0 (Syntax.Term0Parens f)),
      Syntax.normalApplicationRhs = (Lists.map (\a -> Syntax.ArgTerm (Syntax.Term1Term0 (Syntax.Term0Parens a))) args)})))))

-- | Build a Coq Term expressing `(t : T)` with the normal cast operator
coqTermCast :: Syntax.Term -> Syntax.Type -> Syntax.Term
coqTermCast t ty =
    Syntax.TermTerm100 (Syntax.Term100Cast (Syntax.TypeCast {
      Syntax.typeCastTerm = (Syntax.Term10OneTerm (Syntax.OneTermTerm1 (Syntax.Term1Term0 (Syntax.Term0Parens t)))),
      Syntax.typeCastType = ty,
      Syntax.typeCastOperator = Syntax.TypeCastOperatorNormal}))

-- | Build a Coq Term that references a (possibly qualified) identifier
coqTermQualid :: String -> Syntax.Term
coqTermQualid s =
    Syntax.TermTerm100 (Syntax.Term100Term10 (Syntax.Term10OneTerm (Syntax.OneTermExplicit (Syntax.QualidAnnotated {
      Syntax.qualidAnnotatedQualid = (coqQualid s),
      Syntax.qualidAnnotatedUnivAnnot = Nothing}))))

coqTypeTerm :: Syntax.Term -> Syntax.Type
coqTypeTerm t = Syntax.Type t

-- | Produce `Axiom name : type.` from a (name, Hydra type) pair
encodeAxiomDefinitionPair :: Environment.CoqEnvironment -> (String, Core.Type) -> Syntax.Sentence
encodeAxiomDefinitionPair env nt =
    Syntax.Sentence {
      Syntax.sentenceComment = Nothing,
      Syntax.sentenceContent = (Syntax.SentenceContentAxiom (Syntax.AxiomDeclaration {
        Syntax.axiomDeclarationName = (coqIdent (Pairs.first nt)),
        Syntax.axiomDeclarationType = (coqTypeTerm (encodeType env (Pairs.second nt)))}))}

-- | Map a Haskell-`show`n Double/Scientific to a Coq term, routing NaN/Inf to base-lib axioms
encodeFloatLiteral :: String -> Syntax.Term
encodeFloatLiteral s =
    Logic.ifElse (Equality.equal s "Infinity") (coqTermQualid "hydra_posInf") (Logic.ifElse (Equality.equal s "-Infinity") (coqTermQualid "hydra_negInf") (Logic.ifElse (Equality.equal s "NaN") (coqTermQualid "hydra_nan") (coqTermQualid (Strings.cat [
      "(",
      s,
      ")"]))))

-- | Encode a Lambda into a Coq `fun` expression, sanitising the parameter name
encodeLambdaTerm :: Environment.CoqEnvironment -> Core.Lambda -> Syntax.Term
encodeLambdaTerm env lam =

      let paramName = sanitizeVar (Core.unName (Core.lambdaParameter lam))
          binder =
                  Maybes.maybe (Syntax.BinderName (coqName paramName)) (\domTy -> Syntax.BinderType (Syntax.TypeBinders {
                    Syntax.typeBindersNames = [
                      coqName paramName],
                    Syntax.typeBindersType = (coqTypeTerm (encodeType env domTy))})) (Core.lambdaDomain lam)
      in (Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
        Syntax.funBinders = (Syntax.OpenBindersBinders [
          binder]),
        Syntax.funBody = (encodeTerm env (Core.lambdaBody lam))})))

-- | Translate a Hydra literal into its Coq stdlib form, with disambiguating parentheses
encodeLiteral :: Core.Literal -> Syntax.Term
encodeLiteral lit =
    case lit of
      Core.LiteralBoolean v0 -> Logic.ifElse v0 (coqTermQualid "true") (coqTermQualid "false")
      Core.LiteralDecimal v0 -> coqTermQualid (Strings.cat [
        "(",
        (Literals.showDecimal v0),
        ")"])
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueBigfloat v1 -> encodeFloatLiteral (Literals.showBigfloat v1)
        Core.FloatValueFloat32 v1 -> encodeFloatLiteral (Literals.showBigfloat (Literals.float32ToBigfloat v1))
        Core.FloatValueFloat64 v1 -> encodeFloatLiteral (Literals.showBigfloat (Literals.float64ToBigfloat v1))
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueBigint v1 -> coqTermQualid (Strings.cat [
          "(",
          (Literals.showBigint v1),
          ")%Z"])
        Core.IntegerValueInt8 v1 -> coqTermQualid (Strings.cat [
          "(",
          (Literals.showInt8 v1),
          ")%Z"])
        Core.IntegerValueInt16 v1 -> coqTermQualid (Strings.cat [
          "(",
          (Literals.showInt16 v1),
          ")%Z"])
        Core.IntegerValueInt32 v1 -> coqTermQualid (Strings.cat [
          "(",
          (Literals.showInt32 v1),
          ")%Z"])
        Core.IntegerValueInt64 v1 -> coqTermQualid (Strings.cat [
          "(",
          (Literals.showInt64 v1),
          ")%Z"])
        Core.IntegerValueUint8 v1 -> coqTermQualid (Strings.cat [
          "(",
          (Literals.showUint8 v1),
          ")"])
        Core.IntegerValueUint16 v1 -> coqTermQualid (Strings.cat [
          "(",
          (Literals.showUint16 v1),
          ")"])
        Core.IntegerValueUint32 v1 -> coqTermQualid (Strings.cat [
          "(",
          (Literals.showUint32 v1),
          ")"])
        Core.IntegerValueUint64 v1 -> coqTermQualid (Strings.cat [
          "(",
          (Literals.showUint64 v1),
          ")"])
      Core.LiteralString v0 -> coqTermQualid (Strings.cat [
        "\"",
        (escapeCoqString v0),
        "\"%string"])
      Core.LiteralBinary _ -> coqTermQualid "\"\""

-- | Map a Hydra LiteralType to its Coq stdlib counterpart
encodeLiteralType :: Core.LiteralType -> Syntax.Term
encodeLiteralType lt =
    case lt of
      Core.LiteralTypeBoolean -> coqTermQualid "bool"
      Core.LiteralTypeDecimal -> coqTermQualid "Q"
      Core.LiteralTypeFloat _ -> coqTermQualid "Q"
      Core.LiteralTypeInteger v0 -> case v0 of
        Core.IntegerTypeBigint -> coqTermQualid "Z"
        Core.IntegerTypeInt8 -> coqTermQualid "Z"
        Core.IntegerTypeInt16 -> coqTermQualid "Z"
        Core.IntegerTypeInt32 -> coqTermQualid "Z"
        Core.IntegerTypeInt64 -> coqTermQualid "Z"
        Core.IntegerTypeUint8 -> coqTermQualid "nat"
        Core.IntegerTypeUint16 -> coqTermQualid "nat"
        Core.IntegerTypeUint32 -> coqTermQualid "nat"
        Core.IntegerTypeUint64 -> coqTermQualid "nat"
      Core.LiteralTypeString -> coqTermQualid "string"
      Core.LiteralTypeBinary -> coqTermQualid "string"

-- | Translate a Hydra record projection into a Coq lambda that pulls out the field
encodeProjectionElim :: Environment.CoqEnvironment -> Core.Projection -> Syntax.Term
encodeProjectionElim env p =

      let fname = Core.projectionField p
          rawFname = Core.unName fname
          sanitizedSet = Environment.coqEnvironmentSanitizedAccessors env
      in (Logic.ifElse (Sets.member rawFname sanitizedSet) (Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
        Syntax.funBinders = (Syntax.OpenBindersBinders [
          Syntax.BinderName (coqName "_")]),
        Syntax.funBody = (coqTermQualid "hydra_unreachable")}))) (Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
        Syntax.funBinders = (Syntax.OpenBindersBinders [
          Syntax.BinderName (coqName "r_")]),
        Syntax.funBody = (coqTermApp (coqTermQualid rawFname) [
          coqTermQualid "r_"])}))))

-- | Translate a Hydra Term into its Coq Term counterpart. The environment provides the constructor-count map used by encodeUnionElim (to decide whether a match is exhaustive) and the ambiguous-name set used by resolveQualifiedName (to decide whether cross-module references need to stay fully qualified).
encodeTerm :: Environment.CoqEnvironment -> Core.Term -> Syntax.Term
encodeTerm env tm =
    case tm of
      Core.TermAnnotated v0 -> encodeTerm env (Core.annotatedTermBody v0)
      Core.TermApplication v0 -> coqTermApp (encodeTerm env (Core.applicationFunction v0)) [
        encodeTerm env (Core.applicationArgument v0)]
      Core.TermCases v0 -> encodeUnionElim env v0
      Core.TermEither v0 -> Eithers.either (\l -> coqTermApp (coqTermQualid "inl") [
        encodeTerm env l]) (\r -> coqTermApp (coqTermQualid "inr") [
        encodeTerm env r]) v0
      Core.TermInject v0 ->
        let uname = Core.injectionTypeName v0
            ufield = Core.injectionField v0
            fname = Core.fieldName ufield
            fterm = Core.fieldTerm ufield
            constrName = unionConstructorName (Core.unName uname) (Core.unName fname)
        in (coqTermApp (coqTermQualid (resolveQualifiedName env constrName)) [
          encodeTerm env fterm])
      Core.TermLambda v0 -> encodeLambdaTerm env v0
      Core.TermLet v0 ->
        let bindings = Core.letBindings v0
            body = Core.letBody v0
        in (Lists.foldr (\binding -> \acc ->
          let bname = Core.bindingName binding
              bterm = Core.bindingTerm binding
              safeName = sanitizeVar (Core.unName bname)
              recursive = termReferencesVar bname bterm
              recBody =
                      coqTermApp (coqTermQualid "hydra_fix") [
                        Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
                          Syntax.funBinders = (Syntax.OpenBindersBinders [
                            Syntax.BinderName (coqName safeName)]),
                          Syntax.funBody = (encodeTerm env bterm)}))]
              boundTerm = Logic.ifElse recursive recBody (encodeTerm env bterm)
          in (Syntax.TermLet (Syntax.Let {
            Syntax.letBindings = (Syntax.LetBindingsNamed (Syntax.LetNamed {
              Syntax.letNamedBinder = Syntax.LetBinder {
                Syntax.letBinderName = (coqName safeName),
                Syntax.letBinderType = Nothing,
                Syntax.letBinderTerm = boundTerm},
              Syntax.letNamedBinders = []})),
            Syntax.letIn = acc}))) (encodeTerm env body) bindings)
      Core.TermList v0 -> Lists.foldr (\el -> \acc -> coqTermApp (coqTermQualid "cons") [
        encodeTerm env el,
        acc]) (coqTermQualid "nil") v0
      Core.TermLiteral v0 -> encodeLiteral v0
      Core.TermMap _ -> coqTermQualid "nil"
      Core.TermMaybe v0 -> Maybes.maybe (coqTermQualid "None") (\v -> coqTermApp (coqTermQualid "Some") [
        encodeTerm env v]) v0
      Core.TermPair v0 -> coqTermApp (coqTermQualid "pair") [
        encodeTerm env (Pairs.first v0),
        (encodeTerm env (Pairs.second v0))]
      Core.TermProject v0 -> encodeProjectionElim env v0
      Core.TermRecord v0 ->
        let rname = Core.recordTypeName v0
            rfields = Core.recordFields v0
        in (Logic.ifElse (Lists.null rfields) (coqTermQualid "tt") (coqTermApp (coqTermQualid (resolveQualifiedName env (Strings.cat2 "Build_" (Core.unName rname)))) (Lists.map (\f -> encodeTerm env (Core.fieldTerm f)) rfields)))
      Core.TermSet _ -> coqTermQualid "nil"
      Core.TermTypeApplication v0 ->
        let body = Core.typeApplicationTermBody v0
            tyArg = Core.typeApplicationTermType v0
            encoded = encodeTerm env body
            isGround = Sets.null (Utils.collectFreeTypeVarsInType tyArg)
        in (Logic.ifElse (Logic.not isGround) encoded (case body of
          Core.TermMaybe v1 -> Maybes.maybe (coqTermCast (coqTermQualid "None") (coqTypeTerm (coqTermApp (coqTermQualid "option") [
            encodeType env tyArg]))) (\_ -> encoded) v1
          Core.TermList v1 -> Logic.ifElse (Logic.and (Lists.null v1) (case tyArg of
            Core.TypeEither _ -> True
            Core.TypePair _ -> True
            Core.TypeMap _ -> True
            _ -> False)) (coqTermCast (coqTermQualid "nil") (coqTypeTerm (coqTermApp (coqTermQualid "list") [
            encodeType env tyArg]))) encoded
          Core.TermEither _ -> case tyArg of
            Core.TypeEither v2 ->
              let sumTy =
                      coqTypeTerm (coqTermApp (coqTermQualid "sum") [
                        encodeType env (Core.eitherTypeLeft v2),
                        (encodeType env (Core.eitherTypeRight v2))])
              in (coqTermCast encoded sumTy)
            _ -> encoded
          Core.TermTypeApplication v1 ->
            let innerBody = Core.typeApplicationTermBody v1
                innerTyArg = Core.typeApplicationTermType v1
                innerEncoded = encodeTerm env innerBody
            in case innerBody of
              Core.TermEither _ ->
                let sumTy =
                        coqTypeTerm (coqTermApp (coqTermQualid "sum") [
                          encodeType env innerTyArg,
                          (encodeType env tyArg)])
                in (coqTermCast innerEncoded sumTy)
              _ -> encoded
          _ -> encoded))
      Core.TermTypeLambda v0 -> encodeTerm env (Core.typeLambdaBody v0)
      Core.TermUnit -> coqTermQualid "tt"
      Core.TermUnwrap v0 -> encodeWrapElim v0
      Core.TermVariable v0 -> coqTermQualid (resolveQualifiedName env (Core.unName v0))
      Core.TermWrap v0 -> encodeTerm env (Core.wrappedTermBody v0)

-- | Build a Coq `Definition name := body.` sentence from a Hydra term
encodeTermDefinition :: Environment.CoqEnvironment -> String -> Core.Term -> Syntax.SentenceContent
encodeTermDefinition env name body =
    Syntax.SentenceContentDefinition (Syntax.Definition {
      Syntax.definitionLocality = Nothing,
      Syntax.definitionName = (coqIdent name),
      Syntax.definitionBinders = [],
      Syntax.definitionType = Nothing,
      Syntax.definitionBody = (encodeTerm env body)})

-- | Wrap encodeTermDefinition in a Coq Sentence with no leading comment
encodeTermDefinitionPair :: Environment.CoqEnvironment -> (String, Core.Term) -> Syntax.Sentence
encodeTermDefinitionPair env ed =
    Syntax.Sentence {
      Syntax.sentenceComment = Nothing,
      Syntax.sentenceContent = (encodeTermDefinition env (Pairs.first ed) (Pairs.second ed))}

-- | Translate a Hydra Type into a Coq Term representing that type. The environment is consulted to resolve qualified type variable references
encodeType :: Environment.CoqEnvironment -> Core.Type -> Syntax.Term
encodeType env ty =
    case ty of
      Core.TypeAnnotated v0 -> encodeType env (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> coqTermApp (encodeType env (Core.applicationTypeFunction v0)) [
        encodeType env (Core.applicationTypeArgument v0)]
      Core.TypeEither v0 -> coqTermApp (coqTermQualid "sum") [
        encodeType env (Core.eitherTypeLeft v0),
        (encodeType env (Core.eitherTypeRight v0))]
      Core.TypeForall v0 -> Syntax.TermForallOrFun (Syntax.ForallOrFunForall (Syntax.Forall {
        Syntax.forallBinders = (Syntax.OpenBindersBinders [
          Syntax.BinderType (Syntax.TypeBinders {
            Syntax.typeBindersNames = [
              coqName (Core.unName (Core.forallTypeParameter v0))],
            Syntax.typeBindersType = (coqTypeTerm (coqTermQualid "Type"))})]),
        Syntax.forallType = (coqTypeTerm (encodeType env (Core.forallTypeBody v0)))}))
      Core.TypeFunction v0 -> coqArrow (encodeType env (Core.functionTypeDomain v0)) (encodeType env (Core.functionTypeCodomain v0))
      Core.TypeList v0 -> coqTermApp (coqTermQualid "list") [
        encodeType env v0]
      Core.TypeLiteral v0 -> encodeLiteralType v0
      Core.TypeMap v0 -> coqTermApp (coqTermQualid "list") [
        coqTermApp (coqTermQualid "prod") [
          encodeType env (Core.mapTypeKeys v0),
          (encodeType env (Core.mapTypeValues v0))]]
      Core.TypeMaybe v0 -> coqTermApp (coqTermQualid "option") [
        encodeType env v0]
      Core.TypePair v0 -> coqTermApp (coqTermQualid "prod") [
        encodeType env (Core.pairTypeFirst v0),
        (encodeType env (Core.pairTypeSecond v0))]
      Core.TypeRecord _ -> coqTermQualid "unit"
      Core.TypeSet v0 -> coqTermApp (coqTermQualid "list") [
        encodeType env v0]
      Core.TypeUnion _ -> coqTermQualid "unit"
      Core.TypeUnit -> coqTermQualid "unit"
      Core.TypeVariable v0 ->
        let raw = Core.unName v0
            headSeg = Maybes.fromMaybe raw (Lists.maybeHead (Strings.splitOn "." raw))
        in (Logic.ifElse (Logic.or (Equality.equal headSeg "hydra") (Equality.equal headSeg "Build_hydra")) (coqTermQualid (resolveQualifiedName env raw)) (coqTermQualid raw))
      Core.TypeVoid -> coqTermQualid "Empty_set"
      Core.TypeWrap v0 -> encodeType env v0

-- | Build a Coq `Definition name : Type := body.` sentence from a Hydra type
encodeTypeDefinition :: Environment.CoqEnvironment -> String -> Core.Type -> Syntax.SentenceContent
encodeTypeDefinition env name ty =
    Syntax.SentenceContentDefinition (Syntax.Definition {
      Syntax.definitionLocality = Nothing,
      Syntax.definitionName = (coqIdent name),
      Syntax.definitionBinders = [],
      Syntax.definitionType = (Just (coqTypeTerm (coqTermQualid "Type"))),
      Syntax.definitionBody = (encodeType env ty)})

-- | Wrap encodeTypeDefinition in a Coq Sentence with no leading comment
encodeTypeDefinitionPair :: Environment.CoqEnvironment -> (String, Core.Type) -> Syntax.Sentence
encodeTypeDefinitionPair env td =
    Syntax.Sentence {
      Syntax.sentenceComment = Nothing,
      Syntax.sentenceContent = (encodeTypeDefinition env (Pairs.first td) (Pairs.second td))}

-- | Construct a Coq Inductive Constructor line `Name_Tag : body -> Name` for a union variant
encodeUnionConstructor :: Environment.CoqEnvironment -> String -> Core.FieldType -> Syntax.Constructor
encodeUnionConstructor env typeName f =

      let ufn = Core.fieldTypeName f
          uft = Core.fieldTypeType f
          constrName =
                  Strings.cat [
                    typeName,
                    "_",
                    (Formatting.capitalize (Core.unName ufn))]
      in Syntax.Constructor {
        Syntax.constructorName = (coqIdent constrName),
        Syntax.constructorBinders = [],
        Syntax.constructorType = (Just (coqTypeTerm (coqArrow (encodeType env uft) (coqTermQualid typeName))))}

-- | Build a Coq match expression from a Hydra union eliminator. Uses the constructor-count map in the environment to decide whether the match is exhaustive: if so, an explicit default is suppressed; if not and the kernel didn't provide one, inserts `| _ => hydra_unreachable`.
encodeUnionElim :: Environment.CoqEnvironment -> Core.CaseStatement -> Syntax.Term
encodeUnionElim env cs =

      let csName = Core.caseStatementTypeName cs
          csCases = Core.caseStatementCases cs
          csDefault = Core.caseStatementDefault cs
          csLocalName = localTypeName (Core.unName csName)
          expectedCount = Maps.lookup csLocalName (Environment.coqEnvironmentConstructorCounts env)
          caseCount = Lists.length csCases
          baseEqs =
                  Lists.map (\c ->
                    let cfn = Core.fieldName c
                        cft = Core.fieldTerm c
                        constr = resolveQualifiedName env (unionConstructorName (Core.unName csName) (Core.unName cfn))
                    in (Logic.ifElse (isUnitLambda cft) (Syntax.Equation {
                      Syntax.equationPattern = [
                        [
                          Syntax.PatternPattern (Syntax.Pattern10Qualiid (Syntax.Pattern10_Qualid {
                            Syntax.pattern10_QualidQualid = (coqQualid constr),
                            Syntax.pattern10_QualidPatterns = [
                              Syntax.Pattern1 {
                                Syntax.pattern1Pattern = (Syntax.Pattern0Qualid (coqQualid "_")),
                                Syntax.pattern1Scope = Nothing}]}))]],
                      Syntax.equationTerm = (encodeTerm env (unitLambdaBody cft))}) (Syntax.Equation {
                      Syntax.equationPattern = [
                        [
                          Syntax.PatternPattern (Syntax.Pattern10Qualiid (Syntax.Pattern10_Qualid {
                            Syntax.pattern10_QualidQualid = (coqQualid constr),
                            Syntax.pattern10_QualidPatterns = [
                              Syntax.Pattern1 {
                                Syntax.pattern1Pattern = (Syntax.Pattern0Qualid (coqQualid "v_")),
                                Syntax.pattern1Scope = Nothing}]}))]],
                      Syntax.equationTerm = (coqTermApp (encodeTerm env cft) [
                        coqTermQualid "v_"])}))) csCases
          wildcardEq =
                  \body -> Syntax.Equation {
                    Syntax.equationPattern = [
                      [
                        Syntax.PatternPattern (Syntax.Pattern10Qualiid (Syntax.Pattern10_Qualid {
                          Syntax.pattern10_QualidQualid = (coqQualid "_"),
                          Syntax.pattern10_QualidPatterns = []}))]],
                    Syntax.equationTerm = body}
          defaultEqs =
                  Maybes.maybe (Logic.ifElse (Maybes.maybe False (\n -> Logic.not (Equality.gte caseCount n)) expectedCount) [
                    wildcardEq (coqTermQualid "hydra_unreachable")] []) (\defT -> Logic.ifElse (Maybes.maybe False (\n -> Equality.gte caseCount n) expectedCount) [] [
                    wildcardEq (encodeTerm env defT)]) csDefault
          allEqs = Lists.concat2 baseEqs defaultEqs
      in (Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
        Syntax.funBinders = (Syntax.OpenBindersBinders [
          Syntax.BinderName (coqName "x_")]),
        Syntax.funBody = (Syntax.TermTerm100 (Syntax.Term100Term10 (Syntax.Term10OneTerm (Syntax.OneTermTerm1 (Syntax.Term1Term0 (Syntax.Term0Match (Syntax.Match {
          Syntax.matchCaseItems = [
            Syntax.CaseItem {
              Syntax.caseItemTerm = (Syntax.Term100Term10 (Syntax.Term10OneTerm (Syntax.OneTermExplicit (Syntax.QualidAnnotated {
                Syntax.qualidAnnotatedQualid = (coqQualid "x_"),
                Syntax.qualidAnnotatedUnivAnnot = Nothing})))),
              Syntax.caseItemAs = Nothing,
              Syntax.caseItemIn = Nothing}],
          Syntax.matchReturn = Nothing,
          Syntax.matchPipe = True,
          Syntax.matchEquations = allEqs})))))))})))

-- | A Hydra wrap eliminator is just the identity on the wrapped object in Coq
encodeWrapElim :: t0 -> Syntax.Term
encodeWrapElim _n =
    Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
      Syntax.funBinders = (Syntax.OpenBindersBinders [
        Syntax.BinderName (coqName "w_")]),
      Syntax.funBody = (coqTermQualid "w_")}))

-- | Escape a string for Coq string literals: double any embedded quotes
escapeCoqString :: String -> String
escapeCoqString s = Strings.intercalate "\"\"" (Strings.splitOn "\"" s)

-- | Collect a chain of leading lambdas as Coq binders, converting type annotations as well
extractLambdaBinders :: Environment.CoqEnvironment -> Core.Term -> [Syntax.Binder]
extractLambdaBinders env tm =
    case tm of
      Core.TermAnnotated v0 -> extractLambdaBinders env (Core.annotatedTermBody v0)
      Core.TermLambda v0 ->
        let param = Core.lambdaParameter v0
            mDomain = Core.lambdaDomain v0
            binder =
                    Maybes.maybe (Syntax.BinderName (coqName (Core.unName param))) (\domTy -> Syntax.BinderType (Syntax.TypeBinders {
                      Syntax.typeBindersNames = [
                        coqName (Core.unName param)],
                      Syntax.typeBindersType = (coqTypeTerm (encodeType env domTy))})) mDomain
        in (Lists.cons binder (extractLambdaBinders env (Core.lambdaBody v0)))
      _ -> []

-- | True if the Maybe Type is the unit type, looking through annotations
isUnitDomain :: Maybe Core.Type -> Bool
isUnitDomain mty =
    Maybes.maybe False (\ty -> case ty of
      Core.TypeUnit -> True
      Core.TypeRecord v0 -> Lists.null v0
      Core.TypeAnnotated v0 -> isUnitDomain (Just (Core.annotatedTypeBody v0))
      _ -> False) mty

-- | Detect a lambda over the unit type whose parameter is not referenced in the body
isUnitLambda :: Core.Term -> Bool
isUnitLambda tm =
    case tm of
      Core.TermAnnotated v0 -> isUnitLambda (Core.annotatedTermBody v0)
      Core.TermLambda v0 ->
        let unused = Logic.not (termReferencesVar (Core.lambdaParameter v0) (Core.lambdaBody v0))
        in (Logic.and (isUnitDomain (Core.lambdaDomain v0)) unused)
      _ -> False

-- | Take the last dot-separated segment of a (possibly) qualified Hydra name and sanitize it
localTypeName :: String -> String
localTypeName s =

      let parts = Strings.splitOn "." s
          localPart = Maybes.fromMaybe s (Lists.maybeLast parts)
      in (sanitizeVar localPart)

-- | Build a Coq Document from lists of type definitions and term definitions
moduleToCoq :: Environment.CoqEnvironment -> [(String, Core.Type)] -> [(String, Core.Term)] -> Syntax.Document
moduleToCoq env typeDefs termDefs =

      let typesSentences = Lists.map (\td -> encodeTypeDefinitionPair env td) typeDefs
          termsSentences = Lists.map (\ed -> encodeTermDefinitionPair env ed) termDefs
      in Syntax.Document {
        Syntax.documentSentences = (Lists.concat [
          [
            standardImports],
          typesSentences,
          termsSentences])}

-- | Rewrite a stripped hydra.lib.<mod>.<func> name to avoid Coq keyword collisions
renameLibKeyword :: String -> String
renameLibKeyword s =
    Logic.ifElse (Equality.equal s "lists.at") "lists.at_" (Logic.ifElse (Equality.equal s "math.mod") "math.mod_" s)

-- | Emit a Coq `Require Import m1 m2 ...` sentence with a `Standard library imports` comment
requireImportSentence :: [String] -> Syntax.Sentence
requireImportSentence mods =
    Syntax.Sentence {
      Syntax.sentenceComment = (Just (Syntax.Comment "Standard library imports")),
      Syntax.sentenceContent = (Syntax.SentenceContentRequireImport (Syntax.RequireImport {
        Syntax.requireImportFrom = Nothing,
        Syntax.requireImportRequire = True,
        Syntax.requireImportQualification = (Just Syntax.ImportQualificationImport),
        Syntax.requireImportModules = (Lists.map (\m -> coqQualid m) mods)}))}

-- | Resolve a (possibly qualified) Hydra identifier to the form that should appear in Coq source
resolveQualifiedName :: Environment.CoqEnvironment -> String -> String
resolveQualifiedName env s =

      let parts = Strings.splitOn "." s
          head1 = Maybes.fromMaybe s (Lists.maybeHead parts)
          currentNs = Environment.coqEnvironmentCurrentNamespace env
          ambig = Environment.coqEnvironmentAmbiguousNames env
      in (Logic.ifElse (Equality.equal head1 "Build_hydra") (Strings.cat2 "Build_" (sanitizeStripped (Maybes.fromMaybe s (Lists.maybeLast parts)))) (Logic.ifElse (Equality.equal head1 "hydra") (
        let rest = Lists.drop 1 parts
            head2 = Maybes.fromMaybe "" (Lists.maybeHead rest)
        in (Logic.ifElse (Equality.equal head2 "lib") (renameLibKeyword (Strings.intercalate "." (Lists.drop 1 rest))) (
          let localRaw = Maybes.fromMaybe s (Lists.maybeLast parts)
              localN = sanitizeStripped localRaw
              sourceNs = Strings.intercalate "." (Maybes.fromMaybe [] (Lists.maybeInit parts))
              isCurrent = Equality.equal sourceNs currentNs
              isAmbig = Sets.member localRaw ambig
              isCollisionProne =
                      Logic.and (Equality.equal (Lists.length parts) 3) (Logic.and (Equality.equal head2 "parsers") (Logic.not isCurrent))
          in (Logic.ifElse (Logic.and isAmbig (Logic.not isCurrent)) (Strings.cat [
            sourceNs,
            ".",
            localN]) (Logic.ifElse isCollisionProne (Strings.cat [
            sanitizeStripped head2,
            ".",
            (sanitizeStripped localRaw)]) localN))))) (sanitizeVar s)))

-- | Append an underscore if a stripped-local-name reference collides with a Coq reserved word
sanitizeStripped :: String -> String
sanitizeStripped s = Formatting.escapeWithUnderscore Language.coqStrippedReservedWords s

-- | Append an underscore if the name collides with a Coq reserved word
sanitizeVar :: String -> String
sanitizeVar s = Formatting.escapeWithUnderscore Language.coqReservedWords s

-- | The Coq stdlib modules plus the hand-written hydra.lib.base axioms
standardImports :: Syntax.Sentence
standardImports =
    requireImportSentence [
      "Stdlib.Strings.String",
      "Stdlib.Lists.List",
      "Stdlib.ZArith.ZArith",
      "Stdlib.QArith.QArith",
      "hydra.lib.base"]

-- | Peel off leading lambdas and annotations, returning the first non-lambda body
stripLambdas :: Core.Term -> Core.Term
stripLambdas tm =
    case tm of
      Core.TermAnnotated v0 -> stripLambdas (Core.annotatedTermBody v0)
      Core.TermLambda v0 -> stripLambdas (Core.lambdaBody v0)
      _ -> tm

-- | Syntactic free-variable check over the shapes encodeTerm walks through
termReferencesVar :: Core.Name -> Core.Term -> Bool
termReferencesVar name tm =
    case tm of
      Core.TermVariable v0 -> Equality.equal v0 name
      Core.TermAnnotated v0 -> termReferencesVar name (Core.annotatedTermBody v0)
      Core.TermApplication v0 -> Logic.or (termReferencesVar name (Core.applicationFunction v0)) (termReferencesVar name (Core.applicationArgument v0))
      Core.TermLambda v0 -> termReferencesVar name (Core.lambdaBody v0)
      Core.TermCases v0 -> Logic.or (Maybes.isJust (Lists.find (\f -> termReferencesVar name (Core.fieldTerm f)) (Core.caseStatementCases v0))) (Maybes.maybe False (\d -> termReferencesVar name d) (Core.caseStatementDefault v0))
      Core.TermLet v0 -> Logic.or (Maybes.isJust (Lists.find (\b -> termReferencesVar name (Core.bindingTerm b)) (Core.letBindings v0))) (termReferencesVar name (Core.letBody v0))
      Core.TermList v0 -> Maybes.isJust (Lists.find (\el -> termReferencesVar name el) v0)
      Core.TermMaybe v0 -> Maybes.maybe False (\el -> termReferencesVar name el) v0
      Core.TermPair v0 -> Logic.or (termReferencesVar name (Pairs.first v0)) (termReferencesVar name (Pairs.second v0))
      Core.TermRecord v0 -> Maybes.isJust (Lists.find (\f -> termReferencesVar name (Core.fieldTerm f)) (Core.recordFields v0))
      Core.TermInject v0 -> termReferencesVar name (Core.fieldTerm (Core.injectionField v0))
      Core.TermEither v0 -> Eithers.either (\l -> termReferencesVar name l) (\r -> termReferencesVar name r) v0
      Core.TermTypeApplication v0 -> termReferencesVar name (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> termReferencesVar name (Core.typeLambdaBody v0)
      Core.TermWrap v0 -> termReferencesVar name (Core.wrappedTermBody v0)
      _ -> False

-- | Combine a type name and field name into a constructor identifier, preserving the namespace prefix
unionConstructorName :: String -> String -> String
unionConstructorName typeName fieldName =

      let parts = Strings.splitOn "." typeName
          localPart = Maybes.fromMaybe typeName (Lists.maybeLast parts)
          prefixParts = Maybes.fromMaybe [] (Lists.maybeInit parts)
          prefix = Logic.ifElse (Lists.null prefixParts) "" (Strings.cat2 (Strings.intercalate "." prefixParts) ".")
          sanitized = sanitizeVar localPart
      in (Strings.cat [
        prefix,
        sanitized,
        "_",
        (Formatting.capitalize fieldName)])

-- | Peel the outer unit lambda off a term, returning the body
unitLambdaBody :: Core.Term -> Core.Term
unitLambdaBody tm =
    case tm of
      Core.TermAnnotated v0 -> unitLambdaBody (Core.annotatedTermBody v0)
      Core.TermLambda v0 -> Core.lambdaBody v0
      _ -> tm
