-- | Coq code generator: converts Hydra type and term modules to Coq source code (.v files).
-- This file is hand-written (moved out of gen-main/ because the DSL source is not expressive
-- enough to describe all the encoding logic — notably sanitizeVar, escapeCoqString, and
-- the recursive-binding hydra_fix wrapping used by encodeTerm's let case).

module Hydra.Coq.Coder where

import qualified Hydra.Core as Core
import qualified Hydra.Coq.Syntax as Syntax
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

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

coqTermApp :: Syntax.Term -> [Syntax.Term] -> Syntax.Term
coqTermApp f args =
    Logic.ifElse (Lists.null args) f (Syntax.TermTerm100 (Syntax.Term100Term10 (Syntax.Term10Application (Syntax.ApplicationNormal (Syntax.NormalApplication {
      Syntax.normalApplicationLhs = (Syntax.Term1Term0 (Syntax.Term0Parens f)),
      Syntax.normalApplicationRhs = (Lists.map (\a -> Syntax.ArgTerm (Syntax.Term1Term0 (Syntax.Term0Parens a))) args)})))))

coqTermQualid :: String -> Syntax.Term
coqTermQualid s =
    Syntax.TermTerm100 (Syntax.Term100Term10 (Syntax.Term10OneTerm (Syntax.OneTermExplicit (Syntax.QualidAnnotated {
      Syntax.qualidAnnotatedQualid = (coqQualid s),
      Syntax.qualidAnnotatedUnivAnnot = Nothing}))))

coqTypeTerm :: Syntax.Term -> Syntax.Type
coqTypeTerm t = Syntax.Type t

encodeProjectionElim :: Core.Projection -> Syntax.Term
encodeProjectionElim v0 =
        let fname = Core.projectionField v0
        in (Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
          Syntax.funBinders = (Syntax.OpenBindersBinders [
            Syntax.BinderName (coqName "r_")]),
          Syntax.funBody = (coqTermApp (coqTermQualid (Core.unName fname)) [
            coqTermQualid "r_"])})))

encodeUnionElim :: Core.CaseStatement -> Syntax.Term
encodeUnionElim v0 =
        let csName = Core.caseStatementTypeName v0
            csCases = Core.caseStatementCases v0
            csDefault = Core.caseStatementDefault v0
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
            Syntax.matchEquations = (Lists.map (\c ->
              let cfn = Core.fieldName c
                  cft = Core.fieldTerm c
                  constr = unionConstructorName (Core.unName csName) (Core.unName cfn)
              in if isUnitLambda cft
                 -- Unit-typed variant: wildcard pattern for unused unit arg
                 then Syntax.Equation {
                   Syntax.equationPattern = [
                     [
                       Syntax.PatternPattern (Syntax.Pattern10Qualiid (Syntax.Pattern10_Qualid {
                         Syntax.pattern10_QualidQualid = (coqQualid constr),
                         Syntax.pattern10_QualidPatterns = [
                           Syntax.Pattern1 {
                             Syntax.pattern1Pattern = (Syntax.Pattern0Qualid (coqQualid "_")),
                             Syntax.pattern1Scope = Nothing}]}))]],
                   Syntax.equationTerm = encodeTerm (unitLambdaBody cft)}
                 -- Normal variant: bind v_ and apply
                 else Syntax.Equation {
                   Syntax.equationPattern = [
                     [
                       Syntax.PatternPattern (Syntax.Pattern10Qualiid (Syntax.Pattern10_Qualid {
                         Syntax.pattern10_QualidQualid = (coqQualid constr),
                         Syntax.pattern10_QualidPatterns = [
                           Syntax.Pattern1 {
                             Syntax.pattern1Pattern = (Syntax.Pattern0Qualid (coqQualid "v_")),
                             Syntax.pattern1Scope = Nothing}]}))]],
                   Syntax.equationTerm = (coqTermApp (encodeTerm cft) [
                     coqTermQualid "v_"])}) csCases)
                  -- Default case for partial pattern matches
                  ++ (case csDefault of
                    Just defaultTerm -> [Syntax.Equation {
                      Syntax.equationPattern = [
                        [
                          Syntax.PatternPattern (Syntax.Pattern10Qualiid (Syntax.Pattern10_Qualid {
                            Syntax.pattern10_QualidQualid = (coqQualid "_"),
                            Syntax.pattern10_QualidPatterns = []}))]],
                      Syntax.equationTerm = encodeTerm defaultTerm}]
                    Nothing -> [])
                  })))))))})))
encodeWrapElim :: Core.Name -> Syntax.Term
encodeWrapElim _ = Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
        Syntax.funBinders = (Syntax.OpenBindersBinders [
          Syntax.BinderName (coqName "w_")]),
        Syntax.funBody = (coqTermQualid "w_")}))

encodeLambdaTerm :: Core.Lambda -> Syntax.Term
encodeLambdaTerm v0 =
        let param = Core.lambdaParameter v0
            body = Core.lambdaBody v0
            mDomain = Core.lambdaDomain v0
            paramName = sanitizeVar (Core.unName param)
            binder = case mDomain of
              Just domTy -> Syntax.BinderType (Syntax.TypeBinders
                [coqName paramName]
                (Syntax.Type (encodeType domTy)))
              Nothing -> Syntax.BinderName (coqName paramName)
        in (Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
          Syntax.funBinders = (Syntax.OpenBindersBinders [binder]),
          Syntax.funBody = (encodeTerm body)})))

encodeLiteral :: Core.Literal -> Syntax.Term
encodeLiteral lit =
    case lit of
      Core.LiteralBoolean v0 -> Logic.ifElse v0 (coqTermQualid "true") (coqTermQualid "false")
      Core.LiteralFloat v0 -> case v0 of
        Core.FloatValueBigfloat v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showBigfloat v1) ")"))
        Core.FloatValueFloat32 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showBigfloat (Literals.float32ToBigfloat v1)) ")"))
        Core.FloatValueFloat64 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showBigfloat (Literals.float64ToBigfloat v1)) ")"))
      Core.LiteralInteger v0 -> case v0 of
        Core.IntegerValueBigint v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showBigint v1) ")%Z"))
        Core.IntegerValueInt8 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showInt8 v1) ")%Z"))
        Core.IntegerValueInt16 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showInt16 v1) ")%Z"))
        Core.IntegerValueInt32 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showInt32 v1) ")%Z"))
        Core.IntegerValueInt64 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showInt64 v1) ")%Z"))
        Core.IntegerValueUint8 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showUint8 v1) ")"))
        Core.IntegerValueUint16 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showUint16 v1) ")"))
        Core.IntegerValueUint32 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showUint32 v1) ")"))
        Core.IntegerValueUint64 v1 -> coqTermQualid (Strings.cat2 "(" (Strings.cat2 (Literals.showUint64 v1) ")"))
      Core.LiteralString v0 -> coqTermQualid (Strings.cat [
        "\"",
        escapeCoqString v0,
        "\"%string"])
      Core.LiteralBinary _ -> coqTermQualid "\"\""

encodeLiteralType :: Core.LiteralType -> Syntax.Term
encodeLiteralType lt =
    case lt of
      Core.LiteralTypeBoolean -> coqTermQualid "bool"
      Core.LiteralTypeFloat v0 -> case v0 of
        Core.FloatTypeBigfloat -> coqTermQualid "Q"
        Core.FloatTypeFloat32 -> coqTermQualid "Q"
        Core.FloatTypeFloat64 -> coqTermQualid "Q"
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

encodeTerm :: Core.Term -> Syntax.Term
encodeTerm tm =
    case tm of
      Core.TermAnnotated v0 ->
        let body = Core.annotatedTermBody v0
        in (encodeTerm body)
      Core.TermApplication v0 ->
        let fn = Core.applicationFunction v0
            arg = Core.applicationArgument v0
        in (coqTermApp (encodeTerm fn) [
          encodeTerm arg])
      Core.TermEither v0 -> Eithers.either (\l -> coqTermApp (coqTermQualid "inl") [
        encodeTerm l]) (\r -> coqTermApp (coqTermQualid "inr") [
        encodeTerm r]) v0
      Core.TermLambda v0 -> encodeLambdaTerm v0
      Core.TermProject v0 -> encodeProjectionElim v0
      Core.TermCases v0 -> encodeUnionElim v0
      Core.TermUnwrap v0 -> encodeWrapElim v0
      Core.TermLet v0 ->
        let bindings = Core.letBindings v0
            body = Core.letBody v0
        in (Lists.foldr (\binding -> \acc ->
          let bname = Core.bindingName binding
              bterm = Core.bindingTerm binding
              safeName = sanitizeVar (Core.unName bname)
          in if termReferencesVar bname bterm
             -- Recursive binding: use hydra_fix (fun name => body)
             then Syntax.TermLet (Syntax.Let {
               Syntax.letBindings = Syntax.LetBindingsNamed (Syntax.LetNamed {
                 Syntax.letNamedBinder = Syntax.LetBinder {
                   Syntax.letBinderName = coqName safeName,
                   Syntax.letBinderType = Nothing,
                   Syntax.letBinderTerm = coqTermApp (coqTermQualid "hydra_fix") [
                     Syntax.TermForallOrFun (Syntax.ForallOrFunFun (Syntax.Fun {
                       Syntax.funBinders = Syntax.OpenBindersBinders [
                         Syntax.BinderName (coqName safeName)],
                       Syntax.funBody = encodeTerm bterm}))]},
                 Syntax.letNamedBinders = []}),
               Syntax.letIn = acc})
             -- Non-recursive binding: regular let
             else (Syntax.TermLet (Syntax.Let {
               Syntax.letBindings = (Syntax.LetBindingsNamed (Syntax.LetNamed {
                 Syntax.letNamedBinder = Syntax.LetBinder {
                   Syntax.letBinderName = (coqName safeName),
                   Syntax.letBinderType = Nothing,
                   Syntax.letBinderTerm = (encodeTerm bterm)},
                 Syntax.letNamedBinders = []})),
               Syntax.letIn = acc}))) (encodeTerm body) bindings)
      Core.TermList v0 -> Lists.foldr (\elem -> \acc -> coqTermApp (coqTermQualid "cons") [
        encodeTerm elem,
        acc]) (coqTermQualid "nil") v0
      Core.TermLiteral v0 -> encodeLiteral v0
      Core.TermMap _ -> coqTermQualid "nil"
      Core.TermMaybe v0 -> Maybes.maybe (coqTermQualid "None") (\v -> coqTermApp (coqTermQualid "Some") [
        encodeTerm v]) v0
      Core.TermPair v0 ->
        let fst = Pairs.first v0
            snd = Pairs.second v0
        in (coqTermApp (coqTermQualid "pair") [
          encodeTerm fst,
          (encodeTerm snd)])
      Core.TermRecord v0 ->
        let rname = Core.recordTypeName v0
            rfields = Core.recordFields v0
        in if null rfields
           then coqTermQualid "tt"  -- empty record = unit value
           else (coqTermApp (coqTermQualid (Strings.cat2 "Build_" (Core.unName rname))) (Lists.map (\f ->
             let ft = Core.fieldTerm f
             in (encodeTerm ft)) rfields))
      Core.TermSet _ -> coqTermQualid "nil"
      Core.TermTypeApplication v0 ->
        let tbody = Core.typeApplicationTermBody v0
        in (encodeTerm tbody)
      Core.TermTypeLambda v0 ->
        let tbody = Core.typeLambdaBody v0
        in (encodeTerm tbody)
      Core.TermUnion v0 ->
        let uname = Core.injectionTypeName v0
            ufield = Core.injectionField v0
            fname = Core.fieldName ufield
            fterm = Core.fieldTerm ufield
            constrName = unionConstructorName (Core.unName uname) (Core.unName fname)
        in (coqTermApp (coqTermQualid constrName) [
          encodeTerm fterm])
      Core.TermUnit -> coqTermQualid "tt"
      Core.TermVariable v0 -> coqTermQualid (sanitizeVar (Core.unName v0))
      Core.TermWrap v0 ->
        let wbody = Core.wrappedTermBody v0
        in (encodeTerm wbody)

encodeTermDefinition :: String -> Core.Term -> Syntax.SentenceContent
encodeTermDefinition name body =
    Syntax.SentenceContentDefinition (Syntax.Definition {
      Syntax.definitionLocality = Nothing,
      Syntax.definitionName = (coqIdent name),
      Syntax.definitionBinders = [],
      Syntax.definitionType = Nothing,
      Syntax.definitionBody = (encodeTerm body)})

encodeTermDefinitionPair :: (String, Core.Term) -> Syntax.Sentence
encodeTermDefinitionPair ed =

      let ename = Pairs.first ed
          ebody = Pairs.second ed
      in Syntax.Sentence {
        Syntax.sentenceComment = Nothing,
        Syntax.sentenceContent = (encodeTermDefinition ename ebody)}

encodeType :: Core.Type -> Syntax.Term
encodeType ty =
    case ty of
      Core.TypeAnnotated v0 ->
        let atb = Core.annotatedTypeBody v0
        in (encodeType atb)
      Core.TypeApplication v0 ->
        let atf = Core.applicationTypeFunction v0
            ata = Core.applicationTypeArgument v0
        in (coqTermApp (encodeType atf) [
          encodeType ata])
      Core.TypeEither v0 ->
        let etl = Core.eitherTypeLeft v0
            etr = Core.eitherTypeRight v0
        in (coqTermApp (coqTermQualid "sum") [
          encodeType etl,
          (encodeType etr)])
      Core.TypeForall v0 ->
        let fap = Core.forallTypeParameter v0
            fab = Core.forallTypeBody v0
        in (Syntax.TermForallOrFun (Syntax.ForallOrFunForall (Syntax.Forall {
          Syntax.forallBinders = (Syntax.OpenBindersBinders [
            Syntax.BinderType (Syntax.TypeBinders {
              Syntax.typeBindersNames = [
                coqName (Core.unName fap)],
              Syntax.typeBindersType = (coqTypeTerm (coqTermQualid "Type"))})]),
          Syntax.forallType = (coqTypeTerm (encodeType fab))})))
      Core.TypeFunction v0 ->
        let ftd = Core.functionTypeDomain v0
            ftc = Core.functionTypeCodomain v0
        in (coqArrow (encodeType ftd) (encodeType ftc))
      Core.TypeList v0 -> coqTermApp (coqTermQualid "list") [
        encodeType v0]
      Core.TypeLiteral v0 -> encodeLiteralType v0
      Core.TypeMap v0 ->
        let mtk = Core.mapTypeKeys v0
            mtv = Core.mapTypeValues v0
        in (coqTermApp (coqTermQualid "list") [
          coqTermApp (coqTermQualid "prod") [
            encodeType mtk,
            (encodeType mtv)]])
      Core.TypeMaybe v0 -> coqTermApp (coqTermQualid "option") [
        encodeType v0]
      Core.TypePair v0 ->
        let ptf = Core.pairTypeFirst v0
            pts = Core.pairTypeSecond v0
        in (coqTermApp (coqTermQualid "prod") [
          encodeType ptf,
          (encodeType pts)])
      Core.TypeRecord _ -> coqTermQualid "unit"
      Core.TypeSet v0 -> coqTermApp (coqTermQualid "list") [
        encodeType v0]
      Core.TypeUnion _ -> coqTermQualid "unit"
      Core.TypeUnit -> coqTermQualid "unit"
      Core.TypeVariable v0 -> coqTermQualid (Core.unName v0)
      Core.TypeVoid -> coqTermQualid "Empty_set"
      Core.TypeWrap v0 -> encodeType v0

encodeTypeDefinition :: String -> Core.Type -> Syntax.SentenceContent
encodeTypeDefinition name ty =
    Syntax.SentenceContentDefinition (Syntax.Definition {
      Syntax.definitionLocality = Nothing,
      Syntax.definitionName = (coqIdent name),
      Syntax.definitionBinders = [],
      Syntax.definitionType = (Just (coqTypeTerm (coqTermQualid "Type"))),
      Syntax.definitionBody = (encodeType ty)})

encodeTypeDefinitionPair :: (String, Core.Type) -> Syntax.Sentence
encodeTypeDefinitionPair td =

      let tname = Pairs.first td
          tbody = Pairs.second td
      in Syntax.Sentence {
        Syntax.sentenceComment = Nothing,
        Syntax.sentenceContent = (encodeTypeDefinition tname tbody)}

encodeUnionConstructor :: String -> Core.FieldType -> Syntax.Constructor
encodeUnionConstructor typeName f =

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
        Syntax.constructorType = (Just (coqTypeTerm (coqArrow (encodeType uft) (coqTermQualid typeName))))}

moduleToCoq :: [(String, Core.Type)] -> [(String, Core.Term)] -> Syntax.Document
moduleToCoq typeDefs termDefs =

      let typesSentences = Lists.map (\td -> encodeTypeDefinitionPair td) typeDefs
          termsSentences = Lists.map (\ed -> encodeTermDefinitionPair ed) termDefs
      in Syntax.Document {
        Syntax.documentSentences = (Lists.concat [
          [
            standardImports],
          typesSentences,
          termsSentences])}

requireImportSentence :: [String] -> Syntax.Sentence
requireImportSentence mods =
    Syntax.Sentence {
      Syntax.sentenceComment = (Just (Syntax.Comment "Standard library imports")),
      Syntax.sentenceContent = (Syntax.SentenceContentRequireImport (Syntax.RequireImport {
        Syntax.requireImportFrom = Nothing,
        Syntax.requireImportRequire = True,
        Syntax.requireImportQualification = (Just Syntax.ImportQualificationImport),
        Syntax.requireImportModules = (Lists.map (\m -> coqQualid m) mods)}))}

standardImports :: Syntax.Sentence
standardImports =
    requireImportSentence [
      "Stdlib.Strings.String",
      "Stdlib.Lists.List",
      "Stdlib.ZArith.ZArith",
      "Stdlib.QArith.QArith",
      "hydra.lib.base"]

-- | Build a union constructor name from a (possibly qualified) type name and a field name.
-- Keeps the qualified prefix (for namespace dependency detection), sanitizes the local part,
-- then appends "_" and the capitalized field name.
-- E.g., "hydra.core.Type" + "annotated" -> "hydra.core.Type__Annotated"
-- The stripHydraQualifications post-processor will strip the prefix later.
unionConstructorName :: String -> String -> String
unionConstructorName typeName fieldName =
  let (prefix, localPart) = case break (== '.') (reverse typeName) of
        (rev, _:rest) -> (reverse rest ++ ".", reverse rev)  -- has dots
        _ -> ("", typeName)                                   -- no dots
      sanitized = sanitizeVar localPart
  in Strings.cat [prefix, sanitized, "_", Formatting.capitalize fieldName]

-- | Escape a string for Coq string literals. In Coq, " inside a string is escaped as "".
escapeCoqString :: String -> String
escapeCoqString [] = []
escapeCoqString ('"':cs) = '"' : '"' : escapeCoqString cs
escapeCoqString (c:cs) = c : escapeCoqString cs

-- | Sanitize a variable name to avoid Coq reserved words.
sanitizeVar :: String -> String
sanitizeVar s
  | s `elem` coqKeywords = s ++ "_"
  | otherwise = s
  where
    coqKeywords = ["as", "at", "cofix", "do", "else", "end", "exists", "exists2",
      "fix", "for", "forall", "fun", "if", "IF", "in", "let", "match",
      "mod", "open", "Prop", "return", "Set", "then", "Type", "using", "where", "with",
      "Axiom", "Class", "Coercion", "Context", "Definition", "Fixpoint",
      "Hypothesis", "Inductive", "Instance", "Lemma", "Module", "Notation",
      "Proof", "Qed", "Record", "Require", "Import", "Section", "End",
      "Theorem", "Example", "Variable", "Variables",
      -- Coq stdlib names that appear as Hydra-generated lambda parameter names
      "cons", "pair", "nil",
      -- Names that collide with Hydra kernel function names after namespace stripping.
      -- These are common parameter names in the kernel that also appear as show/formatting
      -- function names (e.g., hydra.show.core.term, hydra.show.core.type).
      "term", "literal", "graph", "element"]

-- | Check if a term is a lambda that ignores its parameter (unit-typed variant handler).
-- Matches patterns like: fun (_ : unit) => body  or  fun (x : unit) => body (where x is unused)
isUnitLambda :: Core.Term -> Bool
isUnitLambda tm = case tm of
  Core.TermAnnotated v -> isUnitLambda (Core.annotatedTermBody v)
  Core.TermLambda lam ->
    let unused = not (termReferencesVar (Core.lambdaParameter lam) (Core.lambdaBody lam))
    in isUnitDomain (Core.lambdaDomain lam) && unused
  _ -> False

-- | Check if a domain type is unit (possibly wrapped in annotations).
isUnitDomain :: Maybe Core.Type -> Bool
isUnitDomain Nothing = False
isUnitDomain (Just ty) = isUnitTy ty
  where
    isUnitTy Core.TypeUnit = True
    isUnitTy (Core.TypeRecord []) = True
    isUnitTy (Core.TypeAnnotated at) = isUnitTy (Core.annotatedTypeBody at)
    isUnitTy _ = False

-- | Extract the body from a unit lambda, skipping the lambda wrapper.
unitLambdaBody :: Core.Term -> Core.Term
unitLambdaBody tm = case tm of
  Core.TermAnnotated v -> unitLambdaBody (Core.annotatedTermBody v)
  Core.TermLambda lam -> Core.lambdaBody lam
  _ -> tm

-- | Check if a term contains a reference to a given variable name.
termReferencesVar :: Core.Name -> Core.Term -> Bool
termReferencesVar name tm = case tm of
  Core.TermVariable v -> v == name
  Core.TermAnnotated v -> termReferencesVar name (Core.annotatedTermBody v)
  Core.TermApplication v ->
    termReferencesVar name (Core.applicationFunction v) ||
    termReferencesVar name (Core.applicationArgument v)
  Core.TermLambda lam -> termReferencesVar name (Core.lambdaBody lam)
  Core.TermCases cs ->
    any (\f -> termReferencesVar name (Core.fieldTerm f)) (Core.caseStatementCases cs) ||
    maybe False (termReferencesVar name) (Core.caseStatementDefault cs)
  Core.TermLet v ->
    any (\b -> termReferencesVar name (Core.bindingTerm b)) (Core.letBindings v) ||
    termReferencesVar name (Core.letBody v)
  Core.TermList v -> any (termReferencesVar name) v
  Core.TermMaybe v -> maybe False (termReferencesVar name) v
  Core.TermPair v -> termReferencesVar name (fst v) || termReferencesVar name (snd v)
  Core.TermRecord v -> any (\f -> termReferencesVar name (Core.fieldTerm f)) (Core.recordFields v)
  Core.TermUnion v -> termReferencesVar name (Core.fieldTerm (Core.injectionField v))
  Core.TermEither v -> either (termReferencesVar name) (termReferencesVar name) v
  Core.TermTypeApplication v -> termReferencesVar name (Core.typeApplicationTermBody v)
  Core.TermTypeLambda v -> termReferencesVar name (Core.typeLambdaBody v)
  Core.TermWrap v -> termReferencesVar name (Core.wrappedTermBody v)
  _ -> False

-- | Extract the leading lambda binders from a term, converting them to Coq Binders.
extractLambdaBinders :: Core.Term -> [Syntax.Binder]
extractLambdaBinders tm = case tm of
  Core.TermAnnotated v -> extractLambdaBinders (Core.annotatedTermBody v)
  Core.TermLambda lam ->
    let param = Core.lambdaParameter lam
        mDomain = Core.lambdaDomain lam
        binder = case mDomain of
          Just domTy -> Syntax.BinderType (Syntax.TypeBinders
            [coqName (Core.unName param)]
            (Syntax.Type (encodeType domTy)))
          Nothing -> Syntax.BinderName (coqName (Core.unName param))
    in binder : extractLambdaBinders (Core.lambdaBody lam)
  _ -> []

-- | Strip the leading lambdas from a term, returning the inner body.
stripLambdas :: Core.Term -> Core.Term
stripLambdas tm = case tm of
  Core.TermAnnotated v -> stripLambdas (Core.annotatedTermBody v)
  Core.TermLambda lam -> stripLambdas (Core.lambdaBody lam)
  _ -> tm
