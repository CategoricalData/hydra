-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.haskell.syntax

module Hydra.Encode.Haskell.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Haskell.Syntax as Syntax
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.haskell.syntax.Alternative
alternative :: Syntax.Alternative -> Core.Term
alternative x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Alternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (pattern (Syntax.alternativePattern x))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (caseRhs (Syntax.alternativeRhs x))},
        Core.Field {
          Core.fieldName = (Core.Name "binds"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map localBindings opt)) (Syntax.alternativeBinds x))}]})
-- | Encoder for hydra.haskell.syntax.ApplicationDeclarationHead
applicationDeclarationHead :: Syntax.ApplicationDeclarationHead -> Core.Term
applicationDeclarationHead x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationDeclarationHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (declarationHead (Syntax.applicationDeclarationHeadFunction x))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (variable (Syntax.applicationDeclarationHeadOperand x))}]})
-- | Encoder for hydra.haskell.syntax.ApplicationExpression
applicationExpression :: Syntax.ApplicationExpression -> Core.Term
applicationExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (expression (Syntax.applicationExpressionFunction x))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (expression (Syntax.applicationExpressionArgument x))}]})
-- | Encoder for hydra.haskell.syntax.ApplicationPattern
applicationPattern :: Syntax.ApplicationPattern -> Core.Term
applicationPattern x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.applicationPatternName x))},
        Core.Field {
          Core.fieldName = (Core.Name "args"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map pattern xs)) (Syntax.applicationPatternArgs x))}]})
-- | Encoder for hydra.haskell.syntax.ApplicationType
applicationType :: Syntax.ApplicationType -> Core.Term
applicationType x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ApplicationType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (type_ (Syntax.applicationTypeContext x))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (type_ (Syntax.applicationTypeArgument x))}]})
-- | Encoder for hydra.haskell.syntax.AsPattern
asPattern :: Syntax.AsPattern -> Core.Term
asPattern x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.AsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.asPatternName x))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (pattern (Syntax.asPatternInner x))}]})
-- | Encoder for hydra.haskell.syntax.CaseExpression
caseExpression :: Syntax.CaseExpression -> Core.Term
caseExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (expression (Syntax.caseExpressionCase x))},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map alternative xs)) (Syntax.caseExpressionAlternatives x))}]})
-- | Encoder for hydra.haskell.syntax.CaseRhs
caseRhs :: Syntax.CaseRhs -> Core.Term
caseRhs x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.CaseRhs"),
      Core.wrappedTermBody = (expression (Syntax.unCaseRhs x))})
-- | Encoder for hydra.haskell.syntax.ClassConstraint
classConstraint :: Syntax.ClassConstraint -> Core.Term
classConstraint x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ClassConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.classConstraintName x))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map type_ xs)) (Syntax.classConstraintTypes x))}]})
-- | Encoder for hydra.haskell.syntax.ConstrainedType
constrainedType :: Syntax.ConstrainedType -> Core.Term
constrainedType x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ConstrainedType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (constraint (Syntax.constrainedTypeCtx x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (type_ (Syntax.constrainedTypeType x))}]})
-- | Encoder for hydra.haskell.syntax.Constraint
constraint :: Syntax.Constraint -> Core.Term
constraint x =
    case x of
      Syntax.ConstraintClass v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "class"),
          Core.fieldTerm = (classConstraint v0)}})
      Syntax.ConstraintTuple v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constraint"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "tuple"),
          Core.fieldTerm = (Core.TermList (Lists.map constraint v0))}})
-- | Encoder for hydra.haskell.syntax.Constructor
constructor :: Syntax.Constructor -> Core.Term
constructor x =
    case x of
      Syntax.ConstructorOrdinary v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constructor"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "ordinary"),
          Core.fieldTerm = (positionalConstructor v0)}})
      Syntax.ConstructorRecord v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Constructor"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "record"),
          Core.fieldTerm = (recordConstructor v0)}})
-- | Encoder for hydra.haskell.syntax.DataDeclaration
dataDeclaration :: Syntax.DataDeclaration -> Core.Term
dataDeclaration x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.DataDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "keyword"),
          Core.fieldTerm = (dataKeyword (Syntax.dataDeclarationKeyword x))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map constraint xs)) (Syntax.dataDeclarationContext x))},
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (declarationHead (Syntax.dataDeclarationHead x))},
        Core.Field {
          Core.fieldName = (Core.Name "constructors"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map constructor xs)) (Syntax.dataDeclarationConstructors x))},
        Core.Field {
          Core.fieldName = (Core.Name "deriving"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map derivingClause xs)) (Syntax.dataDeclarationDeriving x))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Syntax.dataDeclarationComments x))}]})
-- | Encoder for hydra.haskell.syntax.DataKeyword
dataKeyword :: Syntax.DataKeyword -> Core.Term
dataKeyword x =
    case x of
      Syntax.DataKeywordData -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DataKeyword"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "data"),
          Core.fieldTerm = Core.TermUnit}})
      Syntax.DataKeywordNewtype -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DataKeyword"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "newtype"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.haskell.syntax.Declaration
declaration :: Syntax.Declaration -> Core.Term
declaration x =
    case x of
      Syntax.DeclarationData v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "data"),
          Core.fieldTerm = (dataDeclaration v0)}})
      Syntax.DeclarationType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (typeSynonymDeclaration v0)}})
      Syntax.DeclarationValueBinding v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (valueBinding v0)}})
      Syntax.DeclarationTypedBinding v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Declaration"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typedBinding"),
          Core.fieldTerm = (typedBinding v0)}})
-- | Encoder for hydra.haskell.syntax.DeclarationHead
declarationHead :: Syntax.DeclarationHead -> Core.Term
declarationHead x =
    case x of
      Syntax.DeclarationHeadApplication v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DeclarationHead"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "application"),
          Core.fieldTerm = (applicationDeclarationHead v0)}})
      Syntax.DeclarationHeadSimple v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.DeclarationHead"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "simple"),
          Core.fieldTerm = (name v0)}})
-- | Encoder for hydra.haskell.syntax.DerivingClause
derivingClause :: Syntax.DerivingClause -> Core.Term
derivingClause x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.DerivingClause"),
      Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map name xs)) (Syntax.unDerivingClause x))})
-- | Encoder for hydra.haskell.syntax.Export
export :: Syntax.Export -> Core.Term
export x =
    case x of
      Syntax.ExportDeclaration v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Export"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "declaration"),
          Core.fieldTerm = (namedImportExport v0)}})
      Syntax.ExportModule v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Export"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (moduleName v0)}})
-- | Encoder for hydra.haskell.syntax.Expression
expression :: Syntax.Expression -> Core.Term
expression x =
    case x of
      Syntax.ExpressionApplication v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "application"),
          Core.fieldTerm = (applicationExpression v0)}})
      Syntax.ExpressionCase v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (caseExpression v0)}})
      Syntax.ExpressionConstructRecord v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "constructRecord"),
          Core.fieldTerm = (recordExpression v0)}})
      Syntax.ExpressionDo v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "do"),
          Core.fieldTerm = (Core.TermList (Lists.map statement v0))}})
      Syntax.ExpressionIf v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "if"),
          Core.fieldTerm = (ifExpression v0)}})
      Syntax.ExpressionInfixApplication v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "infixApplication"),
          Core.fieldTerm = (infixExpression v0)}})
      Syntax.ExpressionLiteral v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (literal v0)}})
      Syntax.ExpressionLambda v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lambda"),
          Core.fieldTerm = (lambdaExpression v0)}})
      Syntax.ExpressionLeftSection v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "leftSection"),
          Core.fieldTerm = (sectionExpression v0)}})
      Syntax.ExpressionLet v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "let"),
          Core.fieldTerm = (letExpression v0)}})
      Syntax.ExpressionList v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = (Core.TermList (Lists.map expression v0))}})
      Syntax.ExpressionRightSection v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "rightSection"),
          Core.fieldTerm = (sectionExpression v0)}})
      Syntax.ExpressionTuple v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "tuple"),
          Core.fieldTerm = (Core.TermList (Lists.map expression v0))}})
      Syntax.ExpressionTypeSignature v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (typedExpression v0)}})
      Syntax.ExpressionUpdateRecord v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "updateRecord"),
          Core.fieldTerm = (recordUpdateExpression v0)}})
      Syntax.ExpressionVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Expression"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (name v0)}})
-- | Encoder for hydra.haskell.syntax.Field
field :: Syntax.Field -> Core.Term
field x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Field"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.fieldName x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (type_ (Syntax.fieldType x))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Syntax.fieldComments x))}]})
-- | Encoder for hydra.haskell.syntax.FieldUpdate
fieldUpdate :: Syntax.FieldUpdate -> Core.Term
fieldUpdate x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FieldUpdate"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.fieldUpdateName x))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (expression (Syntax.fieldUpdateValue x))}]})
-- | Encoder for hydra.haskell.syntax.FunctionType
functionType :: Syntax.FunctionType -> Core.Term
functionType x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.FunctionType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "domain"),
          Core.fieldTerm = (type_ (Syntax.functionTypeDomain x))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (type_ (Syntax.functionTypeCodomain x))}]})
-- | Encoder for hydra.haskell.syntax.IfExpression
ifExpression :: Syntax.IfExpression -> Core.Term
ifExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.IfExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (expression (Syntax.ifExpressionCondition x))},
        Core.Field {
          Core.fieldName = (Core.Name "then"),
          Core.fieldTerm = (expression (Syntax.ifExpressionThen x))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (expression (Syntax.ifExpressionElse x))}]})
-- | Encoder for hydra.haskell.syntax.Import
import_ :: Syntax.Import -> Core.Term
import_ x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Import"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualified"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Syntax.importQualified x))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (moduleName (Syntax.importModule x))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map moduleName opt)) (Syntax.importAs x))},
        Core.Field {
          Core.fieldName = (Core.Name "spec"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map importSpec opt)) (Syntax.importSpec x))}]})
-- | Encoder for hydra.haskell.syntax.ImportExportSubspec
importExportSubspec :: Syntax.ImportExportSubspec -> Core.Term
importExportSubspec x =
    case x of
      Syntax.ImportExportSubspecAll -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportExportSubspec"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = Core.TermUnit}})
      Syntax.ImportExportSubspecList v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportExportSubspec"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = (Core.TermList (Lists.map name v0))}})
-- | Encoder for hydra.haskell.syntax.ImportModifier
importModifier :: Syntax.ImportModifier -> Core.Term
importModifier x =
    case x of
      Syntax.ImportModifierPattern -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportModifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = Core.TermUnit}})
      Syntax.ImportModifierType -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportModifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.haskell.syntax.ImportSpec
importSpec :: Syntax.ImportSpec -> Core.Term
importSpec x =
    case x of
      Syntax.ImportSpecList v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportSpec"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = (Core.TermList (Lists.map namedImportExport v0))}})
      Syntax.ImportSpecHiding v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ImportSpec"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "hiding"),
          Core.fieldTerm = (Core.TermList (Lists.map namedImportExport v0))}})
-- | Encoder for hydra.haskell.syntax.InfixExpression
infixExpression :: Syntax.InfixExpression -> Core.Term
infixExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (expression (Syntax.infixExpressionLhs x))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (operator (Syntax.infixExpressionOperator x))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (expression (Syntax.infixExpressionRhs x))}]})
-- | Encoder for hydra.haskell.syntax.InfixType
infixType :: Syntax.InfixType -> Core.Term
infixType x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.InfixType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (type_ (Syntax.infixTypeLhs x))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (operator (Syntax.infixTypeOperator x))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (type_ (Syntax.infixTypeRhs x))}]})
-- | Encoder for hydra.haskell.syntax.LambdaExpression
lambdaExpression :: Syntax.LambdaExpression -> Core.Term
lambdaExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LambdaExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map pattern xs)) (Syntax.lambdaExpressionBindings x))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (expression (Syntax.lambdaExpressionInner x))}]})
-- | Encoder for hydra.haskell.syntax.LetExpression
letExpression :: Syntax.LetExpression -> Core.Term
letExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map localBinding xs)) (Syntax.letExpressionBindings x))},
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (expression (Syntax.letExpressionInner x))}]})
-- | Encoder for hydra.haskell.syntax.Literal
literal :: Syntax.Literal -> Core.Term
literal x =
    case x of
      Syntax.LiteralChar v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "char"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 v0)))}})
      Syntax.LiteralDouble v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "double"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 v0)))}})
      Syntax.LiteralFloat v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "float"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 v0)))}})
      Syntax.LiteralInt v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "int"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
      Syntax.LiteralInteger v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "integer"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint v0)))}})
      Syntax.LiteralString v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Literal"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "string"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v0))}})
-- | Encoder for hydra.haskell.syntax.LocalBinding
localBinding :: Syntax.LocalBinding -> Core.Term
localBinding x =
    case x of
      Syntax.LocalBindingSignature v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.LocalBinding"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (typeSignature v0)}})
      Syntax.LocalBindingValue v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.LocalBinding"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (valueBinding v0)}})
-- | Encoder for hydra.haskell.syntax.LocalBindings
localBindings :: Syntax.LocalBindings -> Core.Term
localBindings x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.LocalBindings"),
      Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map localBinding xs)) (Syntax.unLocalBindings x))})
-- | Encoder for hydra.haskell.syntax.Module
module_ :: Syntax.Module -> Core.Term
module_ x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map moduleHead opt)) (Syntax.moduleHead x))},
        Core.Field {
          Core.fieldName = (Core.Name "imports"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map import_ xs)) (Syntax.moduleImports x))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map declaration xs)) (Syntax.moduleDeclarations x))}]})
-- | Encoder for hydra.haskell.syntax.ModuleHead
moduleHead :: Syntax.ModuleHead -> Core.Term
moduleHead x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.ModuleHead"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Syntax.moduleHeadComments x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (moduleName (Syntax.moduleHeadName x))},
        Core.Field {
          Core.fieldName = (Core.Name "exports"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map export xs)) (Syntax.moduleHeadExports x))}]})
-- | Encoder for hydra.haskell.syntax.ModuleName
moduleName :: Syntax.ModuleName -> Core.Term
moduleName x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.ModuleName"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Syntax.unModuleName x))})
-- | Encoder for hydra.haskell.syntax.Name
name :: Syntax.Name -> Core.Term
name x =
    case x of
      Syntax.NameImplicit v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Name"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "implicit"),
          Core.fieldTerm = (qualifiedName v0)}})
      Syntax.NameNormal v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Name"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "normal"),
          Core.fieldTerm = (qualifiedName v0)}})
-- | Encoder for hydra.haskell.syntax.NamePart
namePart :: Syntax.NamePart -> Core.Term
namePart x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.NamePart"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Syntax.unNamePart x))})
-- | Encoder for hydra.haskell.syntax.NamedImportExport
namedImportExport :: Syntax.NamedImportExport -> Core.Term
namedImportExport x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.NamedImportExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "modifier"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map importModifier opt)) (Syntax.namedImportExportModifier x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.namedImportExportName x))},
        Core.Field {
          Core.fieldName = (Core.Name "subspec"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map importExportSubspec opt)) (Syntax.namedImportExportSubspec x))}]})
-- | Encoder for hydra.haskell.syntax.Operator
operator :: Syntax.Operator -> Core.Term
operator x =
    case x of
      Syntax.OperatorBacktick v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Operator"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "backtick"),
          Core.fieldTerm = (qualifiedName v0)}})
      Syntax.OperatorNormal v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Operator"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "normal"),
          Core.fieldTerm = (qualifiedName v0)}})
-- | Encoder for hydra.haskell.syntax.Pattern
pattern :: Syntax.Pattern -> Core.Term
pattern x =
    case x of
      Syntax.PatternApplication v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "application"),
          Core.fieldTerm = (applicationPattern v0)}})
      Syntax.PatternAs v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (asPattern v0)}})
      Syntax.PatternList v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = (Core.TermList (Lists.map pattern v0))}})
      Syntax.PatternLiteral v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "literal"),
          Core.fieldTerm = (literal v0)}})
      Syntax.PatternName v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name v0)}})
      Syntax.PatternRecord v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "record"),
          Core.fieldTerm = (recordPattern v0)}})
      Syntax.PatternTuple v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "tuple"),
          Core.fieldTerm = (Core.TermList (Lists.map pattern v0))}})
      Syntax.PatternTyped v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typed"),
          Core.fieldTerm = (typedPattern v0)}})
      Syntax.PatternWildcard -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Pattern"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "wildcard"),
          Core.fieldTerm = Core.TermUnit}})
-- | Encoder for hydra.haskell.syntax.PatternField
patternField :: Syntax.PatternField -> Core.Term
patternField x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PatternField"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.patternFieldName x))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (pattern (Syntax.patternFieldPattern x))}]})
-- | Encoder for hydra.haskell.syntax.PositionalConstructor
positionalConstructor :: Syntax.PositionalConstructor -> Core.Term
positionalConstructor x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.PositionalConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.positionalConstructorName x))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map type_ xs)) (Syntax.positionalConstructorFields x))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Syntax.positionalConstructorComments x))}]})
-- | Encoder for hydra.haskell.syntax.QualifiedName
qualifiedName :: Syntax.QualifiedName -> Core.Term
qualifiedName x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "qualifiers"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map namePart xs)) (Syntax.qualifiedNameQualifiers x))},
        Core.Field {
          Core.fieldName = (Core.Name "unqualified"),
          Core.fieldTerm = (namePart (Syntax.qualifiedNameUnqualified x))}]})
-- | Encoder for hydra.haskell.syntax.RecordConstructor
recordConstructor :: Syntax.RecordConstructor -> Core.Term
recordConstructor x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordConstructor"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.recordConstructorName x))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map field xs)) (Syntax.recordConstructorFields x))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Syntax.recordConstructorComments x))}]})
-- | Encoder for hydra.haskell.syntax.RecordExpression
recordExpression :: Syntax.RecordExpression -> Core.Term
recordExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.recordExpressionName x))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map fieldUpdate xs)) (Syntax.recordExpressionFields x))}]})
-- | Encoder for hydra.haskell.syntax.RecordPattern
recordPattern :: Syntax.RecordPattern -> Core.Term
recordPattern x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.recordPatternName x))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map patternField xs)) (Syntax.recordPatternFields x))}]})
-- | Encoder for hydra.haskell.syntax.RecordUpdateExpression
recordUpdateExpression :: Syntax.RecordUpdateExpression -> Core.Term
recordUpdateExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.RecordUpdateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (expression (Syntax.recordUpdateExpressionInner x))},
        Core.Field {
          Core.fieldName = (Core.Name "fields"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map fieldUpdate xs)) (Syntax.recordUpdateExpressionFields x))}]})
-- | Encoder for hydra.haskell.syntax.RightHandSide
rightHandSide :: Syntax.RightHandSide -> Core.Term
rightHandSide x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.RightHandSide"),
      Core.wrappedTermBody = (expression (Syntax.unRightHandSide x))})
-- | Encoder for hydra.haskell.syntax.SectionExpression
sectionExpression :: Syntax.SectionExpression -> Core.Term
sectionExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (operator (Syntax.sectionExpressionOperator x))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (expression (Syntax.sectionExpressionExpression x))}]})
-- | Encoder for hydra.haskell.syntax.SimpleValueBinding
simpleValueBinding :: Syntax.SimpleValueBinding -> Core.Term
simpleValueBinding x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.SimpleValueBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (pattern (Syntax.simpleValueBindingPattern x))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (rightHandSide (Syntax.simpleValueBindingRhs x))},
        Core.Field {
          Core.fieldName = (Core.Name "localBindings"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map localBindings opt)) (Syntax.simpleValueBindingLocalBindings x))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Syntax.simpleValueBindingComments x))}]})
-- | Encoder for hydra.haskell.syntax.Statement
statement :: Syntax.Statement -> Core.Term
statement x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.Statement"),
      Core.wrappedTermBody = (expression (Syntax.unStatement x))})
-- | Encoder for hydra.haskell.syntax.Type
type_ :: Syntax.Type -> Core.Term
type_ x =
    case x of
      Syntax.TypeApplication v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "application"),
          Core.fieldTerm = (applicationType v0)}})
      Syntax.TypeCtx v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "ctx"),
          Core.fieldTerm = (constrainedType v0)}})
      Syntax.TypeFunction v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (functionType v0)}})
      Syntax.TypeInfix v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "infix"),
          Core.fieldTerm = (infixType v0)}})
      Syntax.TypeList v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "list"),
          Core.fieldTerm = (type_ v0)}})
      Syntax.TypeTuple v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "tuple"),
          Core.fieldTerm = (Core.TermList (Lists.map type_ v0))}})
      Syntax.TypeVariable v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.Type"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (name v0)}})
-- | Encoder for hydra.haskell.syntax.TypeSignature
typeSignature :: Syntax.TypeSignature -> Core.Term
typeSignature x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (name (Syntax.typeSignatureName x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (type_ (Syntax.typeSignatureType x))}]})
-- | Encoder for hydra.haskell.syntax.TypeSynonymDeclaration
typeSynonymDeclaration :: Syntax.TypeSynonymDeclaration -> Core.Term
typeSynonymDeclaration x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypeSynonymDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (declarationHead (Syntax.typeSynonymDeclarationName x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (type_ (Syntax.typeSynonymDeclarationType x))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Syntax.typeSynonymDeclarationComments x))}]})
-- | Encoder for hydra.haskell.syntax.TypedBinding
typedBinding :: Syntax.TypedBinding -> Core.Term
typedBinding x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeSignature"),
          Core.fieldTerm = (typeSignature (Syntax.typedBindingTypeSignature x))},
        Core.Field {
          Core.fieldName = (Core.Name "valueBinding"),
          Core.fieldTerm = (valueBinding (Syntax.typedBindingValueBinding x))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Syntax.typedBindingComments x))}]})
-- | Encoder for hydra.haskell.syntax.TypedExpression
typedExpression :: Syntax.TypedExpression -> Core.Term
typedExpression x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (expression (Syntax.typedExpressionInner x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (type_ (Syntax.typedExpressionType x))}]})
-- | Encoder for hydra.haskell.syntax.TypedPattern
typedPattern :: Syntax.TypedPattern -> Core.Term
typedPattern x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.haskell.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "inner"),
          Core.fieldTerm = (pattern (Syntax.typedPatternInner x))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (type_ (Syntax.typedPatternType x))}]})
-- | Encoder for hydra.haskell.syntax.ValueBinding
valueBinding :: Syntax.ValueBinding -> Core.Term
valueBinding x =
    case x of
      Syntax.ValueBindingSimple v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.haskell.syntax.ValueBinding"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "simple"),
          Core.fieldTerm = (simpleValueBinding v0)}})
-- | Encoder for hydra.haskell.syntax.Variable
variable :: Syntax.Variable -> Core.Term
variable x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.haskell.syntax.Variable"),
      Core.wrappedTermBody = (name (Syntax.unVariable x))})
