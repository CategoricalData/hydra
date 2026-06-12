-- Note: this is an automatically generated file. Do not edit.
-- | Term decoders for hydra.haskell.syntax

module Hydra.Decode.Haskell.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Errors as Errors
import qualified Hydra.Extract.Core as ExtractCore
import qualified Hydra.Graph as Graph
import qualified Hydra.Haskell.Syntax as Syntax
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Decoder for hydra.haskell.syntax.Alternative
alternative :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Alternative
alternative cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "pattern" pattern fieldMap cx) (\field_pattern -> Eithers.bind (ExtractCore.requireField "rhs" caseRhs fieldMap cx) (\field_rhs -> Eithers.bind (ExtractCore.requireField "binds" (ExtractCore.decodeMaybe localBindings) fieldMap cx) (\field_binds -> Right (Syntax.Alternative {
          Syntax.alternativePattern = field_pattern,
          Syntax.alternativeRhs = field_rhs,
          Syntax.alternativeBinds = field_binds})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.Alternative")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHead :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ApplicationDeclarationHead
applicationDeclarationHead cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "function" declarationHead fieldMap cx) (\field_function -> Eithers.bind (ExtractCore.requireField "operand" variable fieldMap cx) (\field_operand -> Right (Syntax.ApplicationDeclarationHead {
          Syntax.applicationDeclarationHeadFunction = field_function,
          Syntax.applicationDeclarationHeadOperand = field_operand}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.ApplicationDeclarationHead")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ApplicationExpression
applicationExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ApplicationExpression
applicationExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "function" expression fieldMap cx) (\field_function -> Eithers.bind (ExtractCore.requireField "argument" expression fieldMap cx) (\field_argument -> Right (Syntax.ApplicationExpression {
          Syntax.applicationExpressionFunction = field_function,
          Syntax.applicationExpressionArgument = field_argument}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.ApplicationExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ApplicationPattern
applicationPattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ApplicationPattern
applicationPattern cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "args" (ExtractCore.decodeList pattern) fieldMap cx) (\field_args -> Right (Syntax.ApplicationPattern {
          Syntax.applicationPatternName = field_name,
          Syntax.applicationPatternArgs = field_args}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.ApplicationPattern")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ApplicationType
applicationType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ApplicationType
applicationType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "context" type_ fieldMap cx) (\field_context -> Eithers.bind (ExtractCore.requireField "argument" type_ fieldMap cx) (\field_argument -> Right (Syntax.ApplicationType {
          Syntax.applicationTypeContext = field_context,
          Syntax.applicationTypeArgument = field_argument}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.ApplicationType")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.AsPattern
asPattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.AsPattern
asPattern cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "inner" pattern fieldMap cx) (\field_inner -> Right (Syntax.AsPattern {
          Syntax.asPatternName = field_name,
          Syntax.asPatternInner = field_inner}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.AsPattern")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.CaseExpression
caseExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.CaseExpression
caseExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "case" expression fieldMap cx) (\field_case -> Eithers.bind (ExtractCore.requireField "alternatives" (ExtractCore.decodeList alternative) fieldMap cx) (\field_alternatives -> Right (Syntax.CaseExpression {
          Syntax.caseExpressionCase = field_case,
          Syntax.caseExpressionAlternatives = field_alternatives}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.CaseExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.CaseRhs
caseRhs :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.CaseRhs
caseRhs cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Syntax.CaseRhs b) (expression cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ClassConstraint
classConstraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ClassConstraint
classConstraint cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "types" (ExtractCore.decodeList type_) fieldMap cx) (\field_types -> Right (Syntax.ClassConstraint {
          Syntax.classConstraintName = field_name,
          Syntax.classConstraintTypes = field_types}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.ClassConstraint")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ConstrainedType
constrainedType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ConstrainedType
constrainedType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "ctx" constraint fieldMap cx) (\field_ctx -> Eithers.bind (ExtractCore.requireField "type" type_ fieldMap cx) (\field_type -> Right (Syntax.ConstrainedType {
          Syntax.constrainedTypeCtx = field_ctx,
          Syntax.constrainedTypeType = field_type}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.ConstrainedType")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Constraint
constraint :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Constraint
constraint cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "class", (\input -> Eithers.map (\t -> Syntax.ConstraintClass t) (classConstraint cx input))),
                      (Core.Name "tuple", (\input -> Eithers.map (\t -> Syntax.ConstraintTuple t) (ExtractCore.decodeList constraint cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Constructor
constructor :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Constructor
constructor cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "ordinary", (\input -> Eithers.map (\t -> Syntax.ConstructorOrdinary t) (positionalConstructor cx input))),
                      (Core.Name "record", (\input -> Eithers.map (\t -> Syntax.ConstructorRecord t) (recordConstructor cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.DataDeclaration
dataDeclaration :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.DataDeclaration
dataDeclaration cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "keyword" dataKeyword fieldMap cx) (\field_keyword -> Eithers.bind (ExtractCore.requireField "context" (ExtractCore.decodeList constraint) fieldMap cx) (\field_context -> Eithers.bind (ExtractCore.requireField "head" declarationHead fieldMap cx) (\field_head -> Eithers.bind (ExtractCore.requireField "constructors" (ExtractCore.decodeList constructor) fieldMap cx) (\field_constructors -> Eithers.bind (ExtractCore.requireField "deriving" (ExtractCore.decodeList derivingClause) fieldMap cx) (\field_deriving -> Eithers.bind (ExtractCore.requireField "comments" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_comments -> Right (Syntax.DataDeclaration {
          Syntax.dataDeclarationKeyword = field_keyword,
          Syntax.dataDeclarationContext = field_context,
          Syntax.dataDeclarationHead = field_head,
          Syntax.dataDeclarationConstructors = field_constructors,
          Syntax.dataDeclarationDeriving = field_deriving,
          Syntax.dataDeclarationComments = field_comments}))))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.DataDeclaration")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.DataKeyword
dataKeyword :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.DataKeyword
dataKeyword cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "data", (\input -> Eithers.map (\t -> Syntax.DataKeywordData) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "newtype", (\input -> Eithers.map (\t -> Syntax.DataKeywordNewtype) (ExtractCore.decodeUnit cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Declaration
declaration :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Declaration
declaration cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "data", (\input -> Eithers.map (\t -> Syntax.DeclarationData t) (dataDeclaration cx input))),
                      (Core.Name "type", (\input -> Eithers.map (\t -> Syntax.DeclarationType t) (typeSynonymDeclaration cx input))),
                      (Core.Name "valueBinding", (\input -> Eithers.map (\t -> Syntax.DeclarationValueBinding t) (valueBinding cx input))),
                      (Core.Name "typedBinding", (\input -> Eithers.map (\t -> Syntax.DeclarationTypedBinding t) (typedBinding cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.DeclarationHead
declarationHead :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.DeclarationHead
declarationHead cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "application",
                        (\input -> Eithers.map (\t -> Syntax.DeclarationHeadApplication t) (applicationDeclarationHead cx input))),
                      (Core.Name "simple", (\input -> Eithers.map (\t -> Syntax.DeclarationHeadSimple t) (name cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.DerivingClause
derivingClause :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.DerivingClause
derivingClause cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Syntax.DerivingClause b) (ExtractCore.decodeList name cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Export
export :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Export
export cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "declaration", (\input -> Eithers.map (\t -> Syntax.ExportDeclaration t) (namedImportExport cx input))),
                      (Core.Name "module", (\input -> Eithers.map (\t -> Syntax.ExportModule t) (moduleName cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Expression
expression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Expression
expression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "application", (\input -> Eithers.map (\t -> Syntax.ExpressionApplication t) (applicationExpression cx input))),
                      (Core.Name "case", (\input -> Eithers.map (\t -> Syntax.ExpressionCase t) (caseExpression cx input))),
                      (
                        Core.Name "constructRecord",
                        (\input -> Eithers.map (\t -> Syntax.ExpressionConstructRecord t) (recordExpression cx input))),
                      (Core.Name "do", (\input -> Eithers.map (\t -> Syntax.ExpressionDo t) (ExtractCore.decodeList statement cx input))),
                      (Core.Name "if", (\input -> Eithers.map (\t -> Syntax.ExpressionIf t) (ifExpression cx input))),
                      (
                        Core.Name "infixApplication",
                        (\input -> Eithers.map (\t -> Syntax.ExpressionInfixApplication t) (infixExpression cx input))),
                      (Core.Name "literal", (\input -> Eithers.map (\t -> Syntax.ExpressionLiteral t) (literal cx input))),
                      (Core.Name "lambda", (\input -> Eithers.map (\t -> Syntax.ExpressionLambda t) (lambdaExpression cx input))),
                      (Core.Name "leftSection", (\input -> Eithers.map (\t -> Syntax.ExpressionLeftSection t) (sectionExpression cx input))),
                      (Core.Name "let", (\input -> Eithers.map (\t -> Syntax.ExpressionLet t) (letExpression cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Syntax.ExpressionList t) (ExtractCore.decodeList expression cx input))),
                      (Core.Name "rightSection", (\input -> Eithers.map (\t -> Syntax.ExpressionRightSection t) (sectionExpression cx input))),
                      (Core.Name "tuple", (\input -> Eithers.map (\t -> Syntax.ExpressionTuple t) (ExtractCore.decodeList expression cx input))),
                      (Core.Name "typeSignature", (\input -> Eithers.map (\t -> Syntax.ExpressionTypeSignature t) (typedExpression cx input))),
                      (
                        Core.Name "updateRecord",
                        (\input -> Eithers.map (\t -> Syntax.ExpressionUpdateRecord t) (recordUpdateExpression cx input))),
                      (Core.Name "variable", (\input -> Eithers.map (\t -> Syntax.ExpressionVariable t) (name cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Field
field :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Field
field cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "type" type_ fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "comments" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_comments -> Right (Syntax.Field {
          Syntax.fieldName = field_name,
          Syntax.fieldType = field_type,
          Syntax.fieldComments = field_comments})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.Field")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.FieldUpdate
fieldUpdate :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.FieldUpdate
fieldUpdate cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "value" expression fieldMap cx) (\field_value -> Right (Syntax.FieldUpdate {
          Syntax.fieldUpdateName = field_name,
          Syntax.fieldUpdateValue = field_value}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.FieldUpdate")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.FunctionType
functionType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.FunctionType
functionType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "domain" type_ fieldMap cx) (\field_domain -> Eithers.bind (ExtractCore.requireField "codomain" type_ fieldMap cx) (\field_codomain -> Right (Syntax.FunctionType {
          Syntax.functionTypeDomain = field_domain,
          Syntax.functionTypeCodomain = field_codomain}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.FunctionType")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.IfExpression
ifExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.IfExpression
ifExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "condition" expression fieldMap cx) (\field_condition -> Eithers.bind (ExtractCore.requireField "then" expression fieldMap cx) (\field_then -> Eithers.bind (ExtractCore.requireField "else" expression fieldMap cx) (\field_else -> Right (Syntax.IfExpression {
          Syntax.ifExpressionCondition = field_condition,
          Syntax.ifExpressionThen = field_then,
          Syntax.ifExpressionElse = field_else})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.IfExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Import
import_ :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Import
import_ cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "qualified" (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralBoolean v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected boolean literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2)) fieldMap cx) (\field_qualified -> Eithers.bind (ExtractCore.requireField "module" moduleName fieldMap cx) (\field_module -> Eithers.bind (ExtractCore.requireField "as" (ExtractCore.decodeMaybe moduleName) fieldMap cx) (\field_as -> Eithers.bind (ExtractCore.requireField "spec" (ExtractCore.decodeMaybe importSpec) fieldMap cx) (\field_spec -> Right (Syntax.Import {
          Syntax.importQualified = field_qualified,
          Syntax.importModule = field_module,
          Syntax.importAs = field_as,
          Syntax.importSpec = field_spec}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.Import")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ImportExportSubspec
importExportSubspec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ImportExportSubspec
importExportSubspec cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "all", (\input -> Eithers.map (\t -> Syntax.ImportExportSubspecAll) (ExtractCore.decodeUnit cx input))),
                      (
                        Core.Name "list",
                        (\input -> Eithers.map (\t -> Syntax.ImportExportSubspecList t) (ExtractCore.decodeList name cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ImportModifier
importModifier :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ImportModifier
importModifier cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "pattern", (\input -> Eithers.map (\t -> Syntax.ImportModifierPattern) (ExtractCore.decodeUnit cx input))),
                      (Core.Name "type", (\input -> Eithers.map (\t -> Syntax.ImportModifierType) (ExtractCore.decodeUnit cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ImportSpec
importSpec :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ImportSpec
importSpec cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "list",
                        (\input -> Eithers.map (\t -> Syntax.ImportSpecList t) (ExtractCore.decodeList namedImportExport cx input))),
                      (
                        Core.Name "hiding",
                        (\input -> Eithers.map (\t -> Syntax.ImportSpecHiding t) (ExtractCore.decodeList namedImportExport cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.InfixExpression
infixExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.InfixExpression
infixExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "lhs" expression fieldMap cx) (\field_lhs -> Eithers.bind (ExtractCore.requireField "operator" operator fieldMap cx) (\field_operator -> Eithers.bind (ExtractCore.requireField "rhs" expression fieldMap cx) (\field_rhs -> Right (Syntax.InfixExpression {
          Syntax.infixExpressionLhs = field_lhs,
          Syntax.infixExpressionOperator = field_operator,
          Syntax.infixExpressionRhs = field_rhs})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.InfixExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.InfixType
infixType :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.InfixType
infixType cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "lhs" type_ fieldMap cx) (\field_lhs -> Eithers.bind (ExtractCore.requireField "operator" operator fieldMap cx) (\field_operator -> Eithers.bind (ExtractCore.requireField "rhs" type_ fieldMap cx) (\field_rhs -> Right (Syntax.InfixType {
          Syntax.infixTypeLhs = field_lhs,
          Syntax.infixTypeOperator = field_operator,
          Syntax.infixTypeRhs = field_rhs})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.InfixType")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.LambdaExpression
lambdaExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.LambdaExpression
lambdaExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "bindings" (ExtractCore.decodeList pattern) fieldMap cx) (\field_bindings -> Eithers.bind (ExtractCore.requireField "inner" expression fieldMap cx) (\field_inner -> Right (Syntax.LambdaExpression {
          Syntax.lambdaExpressionBindings = field_bindings,
          Syntax.lambdaExpressionInner = field_inner}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.LambdaExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.LetExpression
letExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.LetExpression
letExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "bindings" (ExtractCore.decodeList localBinding) fieldMap cx) (\field_bindings -> Eithers.bind (ExtractCore.requireField "inner" expression fieldMap cx) (\field_inner -> Right (Syntax.LetExpression {
          Syntax.letExpressionBindings = field_bindings,
          Syntax.letExpressionInner = field_inner}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.LetExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Literal
literal :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Literal
literal cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (
                        Core.Name "char",
                        (\input -> Eithers.map (\t -> Syntax.LiteralChar t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueUint16 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected uint16 value")
                            _ -> Left (Errors.DecodingError "expected uint16 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "double",
                        (\input -> Eithers.map (\t -> Syntax.LiteralDouble t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralFloat v2 -> case v2 of
                              Core.FloatValueFloat64 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected float64 value")
                            _ -> Left (Errors.DecodingError "expected float64 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "float",
                        (\input -> Eithers.map (\t -> Syntax.LiteralFloat t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralFloat v2 -> case v2 of
                              Core.FloatValueFloat32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected float32 value")
                            _ -> Left (Errors.DecodingError "expected float32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "int",
                        (\input -> Eithers.map (\t -> Syntax.LiteralInt t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueInt32 v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected int32 value")
                            _ -> Left (Errors.DecodingError "expected int32 literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "integer",
                        (\input -> Eithers.map (\t -> Syntax.LiteralInteger t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralInteger v2 -> case v2 of
                              Core.IntegerValueBigint v3 -> Right v3
                              _ -> Left (Errors.DecodingError "expected bigint value")
                            _ -> Left (Errors.DecodingError "expected bigint literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input)))),
                      (
                        Core.Name "string",
                        (\input -> Eithers.map (\t -> Syntax.LiteralString t) (Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
                          Core.TermLiteral v1 -> case v1 of
                            Core.LiteralString v2 -> Right v2
                            _ -> Left (Errors.DecodingError "expected string literal")
                          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx input))))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.LocalBinding
localBinding :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.LocalBinding
localBinding cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "signature", (\input -> Eithers.map (\t -> Syntax.LocalBindingSignature t) (typeSignature cx input))),
                      (Core.Name "value", (\input -> Eithers.map (\t -> Syntax.LocalBindingValue t) (valueBinding cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.LocalBindings
localBindings :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.LocalBindings
localBindings cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Syntax.LocalBindings b) (ExtractCore.decodeList localBinding cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Module
module_ :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Module
module_ cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "head" (ExtractCore.decodeMaybe moduleHead) fieldMap cx) (\field_head -> Eithers.bind (ExtractCore.requireField "imports" (ExtractCore.decodeList import_) fieldMap cx) (\field_imports -> Eithers.bind (ExtractCore.requireField "declarations" (ExtractCore.decodeList declaration) fieldMap cx) (\field_declarations -> Right (Syntax.Module {
          Syntax.moduleHead = field_head,
          Syntax.moduleImports = field_imports,
          Syntax.moduleDeclarations = field_declarations})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.Module")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ModuleHead
moduleHead :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ModuleHead
moduleHead cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "comments" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_comments -> Eithers.bind (ExtractCore.requireField "name" moduleName fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "exports" (ExtractCore.decodeList export) fieldMap cx) (\field_exports -> Right (Syntax.ModuleHead {
          Syntax.moduleHeadComments = field_comments,
          Syntax.moduleHeadName = field_name,
          Syntax.moduleHeadExports = field_exports})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.ModuleHead")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ModuleName
moduleName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ModuleName
moduleName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Syntax.ModuleName b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Name
name :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Name
name cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "implicit", (\input -> Eithers.map (\t -> Syntax.NameImplicit t) (qualifiedName cx input))),
                      (Core.Name "normal", (\input -> Eithers.map (\t -> Syntax.NameNormal t) (qualifiedName cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.NamePart
namePart :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.NamePart
namePart cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Syntax.NamePart b) ((\raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
        Core.TermLiteral v1 -> case v1 of
          Core.LiteralString v2 -> Right v2
          _ -> Left (Errors.DecodingError "expected string literal")
        _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx raw2)) (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.NamedImportExport
namedImportExport :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.NamedImportExport
namedImportExport cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "modifier" (ExtractCore.decodeMaybe importModifier) fieldMap cx) (\field_modifier -> Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "subspec" (ExtractCore.decodeMaybe importExportSubspec) fieldMap cx) (\field_subspec -> Right (Syntax.NamedImportExport {
          Syntax.namedImportExportModifier = field_modifier,
          Syntax.namedImportExportName = field_name,
          Syntax.namedImportExportSubspec = field_subspec})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.NamedImportExport")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Operator
operator :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Operator
operator cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "backtick", (\input -> Eithers.map (\t -> Syntax.OperatorBacktick t) (qualifiedName cx input))),
                      (Core.Name "normal", (\input -> Eithers.map (\t -> Syntax.OperatorNormal t) (qualifiedName cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Pattern
pattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Pattern
pattern cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "application", (\input -> Eithers.map (\t -> Syntax.PatternApplication t) (applicationPattern cx input))),
                      (Core.Name "as", (\input -> Eithers.map (\t -> Syntax.PatternAs t) (asPattern cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Syntax.PatternList t) (ExtractCore.decodeList pattern cx input))),
                      (Core.Name "literal", (\input -> Eithers.map (\t -> Syntax.PatternLiteral t) (literal cx input))),
                      (Core.Name "name", (\input -> Eithers.map (\t -> Syntax.PatternName t) (name cx input))),
                      (Core.Name "record", (\input -> Eithers.map (\t -> Syntax.PatternRecord t) (recordPattern cx input))),
                      (Core.Name "tuple", (\input -> Eithers.map (\t -> Syntax.PatternTuple t) (ExtractCore.decodeList pattern cx input))),
                      (Core.Name "typed", (\input -> Eithers.map (\t -> Syntax.PatternTyped t) (typedPattern cx input))),
                      (Core.Name "wildcard", (\input -> Eithers.map (\t -> Syntax.PatternWildcard) (ExtractCore.decodeUnit cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.PatternField
patternField :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.PatternField
patternField cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "pattern" pattern fieldMap cx) (\field_pattern -> Right (Syntax.PatternField {
          Syntax.patternFieldName = field_name,
          Syntax.patternFieldPattern = field_pattern}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.PatternField")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.PositionalConstructor
positionalConstructor :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.PositionalConstructor
positionalConstructor cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "fields" (ExtractCore.decodeList type_) fieldMap cx) (\field_fields -> Eithers.bind (ExtractCore.requireField "comments" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_comments -> Right (Syntax.PositionalConstructor {
          Syntax.positionalConstructorName = field_name,
          Syntax.positionalConstructorFields = field_fields,
          Syntax.positionalConstructorComments = field_comments})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.PositionalConstructor")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.QualifiedName
qualifiedName :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.QualifiedName
qualifiedName cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "qualifiers" (ExtractCore.decodeList namePart) fieldMap cx) (\field_qualifiers -> Eithers.bind (ExtractCore.requireField "unqualified" namePart fieldMap cx) (\field_unqualified -> Right (Syntax.QualifiedName {
          Syntax.qualifiedNameQualifiers = field_qualifiers,
          Syntax.qualifiedNameUnqualified = field_unqualified}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.QualifiedName")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.RecordConstructor
recordConstructor :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.RecordConstructor
recordConstructor cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "fields" (ExtractCore.decodeList field) fieldMap cx) (\field_fields -> Eithers.bind (ExtractCore.requireField "comments" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_comments -> Right (Syntax.RecordConstructor {
          Syntax.recordConstructorName = field_name,
          Syntax.recordConstructorFields = field_fields,
          Syntax.recordConstructorComments = field_comments})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.RecordConstructor")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.RecordExpression
recordExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.RecordExpression
recordExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "fields" (ExtractCore.decodeList fieldUpdate) fieldMap cx) (\field_fields -> Right (Syntax.RecordExpression {
          Syntax.recordExpressionName = field_name,
          Syntax.recordExpressionFields = field_fields}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.RecordExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.RecordPattern
recordPattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.RecordPattern
recordPattern cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "fields" (ExtractCore.decodeList patternField) fieldMap cx) (\field_fields -> Right (Syntax.RecordPattern {
          Syntax.recordPatternName = field_name,
          Syntax.recordPatternFields = field_fields}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.RecordPattern")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.RecordUpdateExpression
recordUpdateExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "inner" expression fieldMap cx) (\field_inner -> Eithers.bind (ExtractCore.requireField "fields" (ExtractCore.decodeList fieldUpdate) fieldMap cx) (\field_fields -> Right (Syntax.RecordUpdateExpression {
          Syntax.recordUpdateExpressionInner = field_inner,
          Syntax.recordUpdateExpressionFields = field_fields}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.RecordUpdateExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.RightHandSide
rightHandSide :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.RightHandSide
rightHandSide cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Syntax.RightHandSide b) (expression cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.SectionExpression
sectionExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.SectionExpression
sectionExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "operator" operator fieldMap cx) (\field_operator -> Eithers.bind (ExtractCore.requireField "expression" expression fieldMap cx) (\field_expression -> Right (Syntax.SectionExpression {
          Syntax.sectionExpressionOperator = field_operator,
          Syntax.sectionExpressionExpression = field_expression}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.SectionExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.SimpleValueBinding
simpleValueBinding :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.SimpleValueBinding
simpleValueBinding cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "pattern" pattern fieldMap cx) (\field_pattern -> Eithers.bind (ExtractCore.requireField "rhs" rightHandSide fieldMap cx) (\field_rhs -> Eithers.bind (ExtractCore.requireField "localBindings" (ExtractCore.decodeMaybe localBindings) fieldMap cx) (\field_localBindings -> Eithers.bind (ExtractCore.requireField "comments" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_comments -> Right (Syntax.SimpleValueBinding {
          Syntax.simpleValueBindingPattern = field_pattern,
          Syntax.simpleValueBindingRhs = field_rhs,
          Syntax.simpleValueBindingLocalBindings = field_localBindings,
          Syntax.simpleValueBindingComments = field_comments}))))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.SimpleValueBinding")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Statement
statement :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Statement
statement cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Syntax.Statement b) (expression cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Type
type_ :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Type
type_ cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "application", (\input -> Eithers.map (\t -> Syntax.TypeApplication t) (applicationType cx input))),
                      (Core.Name "ctx", (\input -> Eithers.map (\t -> Syntax.TypeCtx t) (constrainedType cx input))),
                      (Core.Name "function", (\input -> Eithers.map (\t -> Syntax.TypeFunction t) (functionType cx input))),
                      (Core.Name "infix", (\input -> Eithers.map (\t -> Syntax.TypeInfix t) (infixType cx input))),
                      (Core.Name "list", (\input -> Eithers.map (\t -> Syntax.TypeList t) (type_ cx input))),
                      (Core.Name "tuple", (\input -> Eithers.map (\t -> Syntax.TypeTuple t) (ExtractCore.decodeList type_ cx input))),
                      (Core.Name "variable", (\input -> Eithers.map (\t -> Syntax.TypeVariable t) (name cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.TypeSignature
typeSignature :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.TypeSignature
typeSignature cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" name fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "type" type_ fieldMap cx) (\field_type -> Right (Syntax.TypeSignature {
          Syntax.typeSignatureName = field_name,
          Syntax.typeSignatureType = field_type}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.TypeSignature")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclaration :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.TypeSynonymDeclaration
typeSynonymDeclaration cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "name" declarationHead fieldMap cx) (\field_name -> Eithers.bind (ExtractCore.requireField "type" type_ fieldMap cx) (\field_type -> Eithers.bind (ExtractCore.requireField "comments" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_comments -> Right (Syntax.TypeSynonymDeclaration {
          Syntax.typeSynonymDeclarationName = field_name,
          Syntax.typeSynonymDeclarationType = field_type,
          Syntax.typeSynonymDeclarationComments = field_comments})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.TypeSynonymDeclaration")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.TypedBinding
typedBinding :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.TypedBinding
typedBinding cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "typeSignature" typeSignature fieldMap cx) (\field_typeSignature -> Eithers.bind (ExtractCore.requireField "valueBinding" valueBinding fieldMap cx) (\field_valueBinding -> Eithers.bind (ExtractCore.requireField "comments" (ExtractCore.decodeMaybe (\cx2 -> \raw2 -> Eithers.either (\err -> Left err) (\stripped2 -> case stripped2 of
          Core.TermLiteral v1 -> case v1 of
            Core.LiteralString v2 -> Right v2
            _ -> Left (Errors.DecodingError "expected string literal")
          _ -> Left (Errors.DecodingError "expected literal")) (ExtractCore.stripWithDecodingError cx2 raw2))) fieldMap cx) (\field_comments -> Right (Syntax.TypedBinding {
          Syntax.typedBindingTypeSignature = field_typeSignature,
          Syntax.typedBindingValueBinding = field_valueBinding,
          Syntax.typedBindingComments = field_comments})))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.TypedBinding")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.TypedExpression
typedExpression :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.TypedExpression
typedExpression cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "inner" expression fieldMap cx) (\field_inner -> Eithers.bind (ExtractCore.requireField "type" type_ fieldMap cx) (\field_type -> Right (Syntax.TypedExpression {
          Syntax.typedExpressionInner = field_inner,
          Syntax.typedExpressionType = field_type}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.TypedExpression")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.TypedPattern
typedPattern :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.TypedPattern
typedPattern cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermRecord v0 ->
        let fieldMap = ExtractCore.toFieldMap v0
        in (Eithers.bind (ExtractCore.requireField "inner" pattern fieldMap cx) (\field_inner -> Eithers.bind (ExtractCore.requireField "type" type_ fieldMap cx) (\field_type -> Right (Syntax.TypedPattern {
          Syntax.typedPatternInner = field_inner,
          Syntax.typedPatternType = field_type}))))
      _ -> Left (Errors.DecodingError "expected a record of type hydra.haskell.syntax.TypedPattern")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.ValueBinding
valueBinding :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.ValueBinding
valueBinding cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermInject v0 ->
        let field = Core.injectionField v0
            fname = Core.fieldName field
            fterm = Core.fieldTerm field
            variantMap =
                    Maps.fromList [
                      (Core.Name "simple", (\input -> Eithers.map (\t -> Syntax.ValueBindingSimple t) (simpleValueBinding cx input)))]
        in (Optionals.cases (Maps.lookup fname variantMap) (Left (Errors.DecodingError (Strings.cat [
          "no such field ",
          (Core.unName fname),
          " in union"]))) (\f -> f fterm))
      _ -> Left (Errors.DecodingError "expected union")) (ExtractCore.stripWithDecodingError cx raw)
-- | Decoder for hydra.haskell.syntax.Variable
variable :: Graph.Graph -> Core.Term -> Either Errors.DecodingError Syntax.Variable
variable cx raw =
    Eithers.either (\err -> Left err) (\stripped -> case stripped of
      Core.TermWrap v0 -> Eithers.map (\b -> Syntax.Variable b) (name cx (Core.wrappedTermBody v0))
      _ -> Left (Errors.DecodingError "expected wrapped type")) (ExtractCore.stripWithDecodingError cx raw)
