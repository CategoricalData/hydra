-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.haskell.ast

module Hydra.Dsl.Ext.Haskell.Ast where

import qualified Hydra.Ext.Haskell.Ast as Ast
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alternative :: (Ast.Pattern -> Ast.CaseRhs -> Maybe Ast.LocalBindings -> Ast.Alternative)
alternative pattern rhs binds = Ast.Alternative {
  Ast.alternativePattern = pattern,
  Ast.alternativeRhs = rhs,
  Ast.alternativeBinds = binds}

alternativePattern :: (Ast.Alternative -> Ast.Pattern)
alternativePattern = Ast.alternativePattern

alternativeRhs :: (Ast.Alternative -> Ast.CaseRhs)
alternativeRhs = Ast.alternativeRhs

alternativeBinds :: (Ast.Alternative -> Maybe Ast.LocalBindings)
alternativeBinds = Ast.alternativeBinds

alternativeWithPattern :: (Ast.Alternative -> Ast.Pattern -> Ast.Alternative)
alternativeWithPattern original newVal = Ast.Alternative {
  Ast.alternativePattern = newVal,
  Ast.alternativeRhs = (Ast.alternativeRhs original),
  Ast.alternativeBinds = (Ast.alternativeBinds original)}

alternativeWithRhs :: (Ast.Alternative -> Ast.CaseRhs -> Ast.Alternative)
alternativeWithRhs original newVal = Ast.Alternative {
  Ast.alternativePattern = (Ast.alternativePattern original),
  Ast.alternativeRhs = newVal,
  Ast.alternativeBinds = (Ast.alternativeBinds original)}

alternativeWithBinds :: (Ast.Alternative -> Maybe Ast.LocalBindings -> Ast.Alternative)
alternativeWithBinds original newVal = Ast.Alternative {
  Ast.alternativePattern = (Ast.alternativePattern original),
  Ast.alternativeRhs = (Ast.alternativeRhs original),
  Ast.alternativeBinds = newVal}

assertionClass :: (Ast.ClassAssertion -> Ast.Assertion)
assertionClass x = (Ast.AssertionClass x)

assertionTuple :: ([Ast.Assertion] -> Ast.Assertion)
assertionTuple x = (Ast.AssertionTuple x)

classAssertion :: (Ast.Name -> [Ast.Type] -> Ast.ClassAssertion)
classAssertion name types = Ast.ClassAssertion {
  Ast.classAssertionName = name,
  Ast.classAssertionTypes = types}

classAssertionName :: (Ast.ClassAssertion -> Ast.Name)
classAssertionName = Ast.classAssertionName

classAssertionTypes :: (Ast.ClassAssertion -> [Ast.Type])
classAssertionTypes = Ast.classAssertionTypes

classAssertionWithName :: (Ast.ClassAssertion -> Ast.Name -> Ast.ClassAssertion)
classAssertionWithName original newVal = Ast.ClassAssertion {
  Ast.classAssertionName = newVal,
  Ast.classAssertionTypes = (Ast.classAssertionTypes original)}

classAssertionWithTypes :: (Ast.ClassAssertion -> [Ast.Type] -> Ast.ClassAssertion)
classAssertionWithTypes original newVal = Ast.ClassAssertion {
  Ast.classAssertionName = (Ast.classAssertionName original),
  Ast.classAssertionTypes = newVal}

caseRhs :: (Ast.Expression -> Ast.CaseRhs)
caseRhs x = (Ast.CaseRhs x)

unCaseRhs :: (Ast.CaseRhs -> Ast.Expression)
unCaseRhs = Ast.unCaseRhs

constructorOrdinary :: (Ast.OrdinaryConstructor -> Ast.Constructor)
constructorOrdinary x = (Ast.ConstructorOrdinary x)

constructorRecord :: (Ast.RecordConstructor -> Ast.Constructor)
constructorRecord x = (Ast.ConstructorRecord x)

ordinaryConstructor :: (Ast.Name -> [Ast.Type] -> Ast.OrdinaryConstructor)
ordinaryConstructor name fields = Ast.OrdinaryConstructor {
  Ast.ordinaryConstructorName = name,
  Ast.ordinaryConstructorFields = fields}

ordinaryConstructorName :: (Ast.OrdinaryConstructor -> Ast.Name)
ordinaryConstructorName = Ast.ordinaryConstructorName

ordinaryConstructorFields :: (Ast.OrdinaryConstructor -> [Ast.Type])
ordinaryConstructorFields = Ast.ordinaryConstructorFields

ordinaryConstructorWithName :: (Ast.OrdinaryConstructor -> Ast.Name -> Ast.OrdinaryConstructor)
ordinaryConstructorWithName original newVal = Ast.OrdinaryConstructor {
  Ast.ordinaryConstructorName = newVal,
  Ast.ordinaryConstructorFields = (Ast.ordinaryConstructorFields original)}

ordinaryConstructorWithFields :: (Ast.OrdinaryConstructor -> [Ast.Type] -> Ast.OrdinaryConstructor)
ordinaryConstructorWithFields original newVal = Ast.OrdinaryConstructor {
  Ast.ordinaryConstructorName = (Ast.ordinaryConstructorName original),
  Ast.ordinaryConstructorFields = newVal}

recordConstructor :: (Ast.Name -> [Ast.FieldWithComments] -> Ast.RecordConstructor)
recordConstructor name fields = Ast.RecordConstructor {
  Ast.recordConstructorName = name,
  Ast.recordConstructorFields = fields}

recordConstructorName :: (Ast.RecordConstructor -> Ast.Name)
recordConstructorName = Ast.recordConstructorName

recordConstructorFields :: (Ast.RecordConstructor -> [Ast.FieldWithComments])
recordConstructorFields = Ast.recordConstructorFields

recordConstructorWithName :: (Ast.RecordConstructor -> Ast.Name -> Ast.RecordConstructor)
recordConstructorWithName original newVal = Ast.RecordConstructor {
  Ast.recordConstructorName = newVal,
  Ast.recordConstructorFields = (Ast.recordConstructorFields original)}

recordConstructorWithFields :: (Ast.RecordConstructor -> [Ast.FieldWithComments] -> Ast.RecordConstructor)
recordConstructorWithFields original newVal = Ast.RecordConstructor {
  Ast.recordConstructorName = (Ast.recordConstructorName original),
  Ast.recordConstructorFields = newVal}

constructorWithComments :: (Ast.Constructor -> Maybe String -> Ast.ConstructorWithComments)
constructorWithComments body comments = Ast.ConstructorWithComments {
  Ast.constructorWithCommentsBody = body,
  Ast.constructorWithCommentsComments = comments}

constructorWithCommentsBody :: (Ast.ConstructorWithComments -> Ast.Constructor)
constructorWithCommentsBody = Ast.constructorWithCommentsBody

constructorWithCommentsComments :: (Ast.ConstructorWithComments -> Maybe String)
constructorWithCommentsComments = Ast.constructorWithCommentsComments

constructorWithCommentsWithBody :: (Ast.ConstructorWithComments -> Ast.Constructor -> Ast.ConstructorWithComments)
constructorWithCommentsWithBody original newVal = Ast.ConstructorWithComments {
  Ast.constructorWithCommentsBody = newVal,
  Ast.constructorWithCommentsComments = (Ast.constructorWithCommentsComments original)}

constructorWithCommentsWithComments :: (Ast.ConstructorWithComments -> Maybe String -> Ast.ConstructorWithComments)
constructorWithCommentsWithComments original newVal = Ast.ConstructorWithComments {
  Ast.constructorWithCommentsBody = (Ast.constructorWithCommentsBody original),
  Ast.constructorWithCommentsComments = newVal}

dataDeclaration :: (Ast.DataOrNewtype -> [Ast.Assertion] -> Ast.DeclarationHead -> [Ast.ConstructorWithComments] -> [Ast.Deriving] -> Ast.DataDeclaration)
dataDeclaration keyword context head constructors deriving_ = Ast.DataDeclaration {
  Ast.dataDeclarationKeyword = keyword,
  Ast.dataDeclarationContext = context,
  Ast.dataDeclarationHead = head,
  Ast.dataDeclarationConstructors = constructors,
  Ast.dataDeclarationDeriving = deriving_}

dataDeclarationKeyword :: (Ast.DataDeclaration -> Ast.DataOrNewtype)
dataDeclarationKeyword = Ast.dataDeclarationKeyword

dataDeclarationContext :: (Ast.DataDeclaration -> [Ast.Assertion])
dataDeclarationContext = Ast.dataDeclarationContext

dataDeclarationHead :: (Ast.DataDeclaration -> Ast.DeclarationHead)
dataDeclarationHead = Ast.dataDeclarationHead

dataDeclarationConstructors :: (Ast.DataDeclaration -> [Ast.ConstructorWithComments])
dataDeclarationConstructors = Ast.dataDeclarationConstructors

dataDeclarationDeriving :: (Ast.DataDeclaration -> [Ast.Deriving])
dataDeclarationDeriving = Ast.dataDeclarationDeriving

dataDeclarationWithKeyword :: (Ast.DataDeclaration -> Ast.DataOrNewtype -> Ast.DataDeclaration)
dataDeclarationWithKeyword original newVal = Ast.DataDeclaration {
  Ast.dataDeclarationKeyword = newVal,
  Ast.dataDeclarationContext = (Ast.dataDeclarationContext original),
  Ast.dataDeclarationHead = (Ast.dataDeclarationHead original),
  Ast.dataDeclarationConstructors = (Ast.dataDeclarationConstructors original),
  Ast.dataDeclarationDeriving = (Ast.dataDeclarationDeriving original)}

dataDeclarationWithContext :: (Ast.DataDeclaration -> [Ast.Assertion] -> Ast.DataDeclaration)
dataDeclarationWithContext original newVal = Ast.DataDeclaration {
  Ast.dataDeclarationKeyword = (Ast.dataDeclarationKeyword original),
  Ast.dataDeclarationContext = newVal,
  Ast.dataDeclarationHead = (Ast.dataDeclarationHead original),
  Ast.dataDeclarationConstructors = (Ast.dataDeclarationConstructors original),
  Ast.dataDeclarationDeriving = (Ast.dataDeclarationDeriving original)}

dataDeclarationWithHead :: (Ast.DataDeclaration -> Ast.DeclarationHead -> Ast.DataDeclaration)
dataDeclarationWithHead original newVal = Ast.DataDeclaration {
  Ast.dataDeclarationKeyword = (Ast.dataDeclarationKeyword original),
  Ast.dataDeclarationContext = (Ast.dataDeclarationContext original),
  Ast.dataDeclarationHead = newVal,
  Ast.dataDeclarationConstructors = (Ast.dataDeclarationConstructors original),
  Ast.dataDeclarationDeriving = (Ast.dataDeclarationDeriving original)}

dataDeclarationWithConstructors :: (Ast.DataDeclaration -> [Ast.ConstructorWithComments] -> Ast.DataDeclaration)
dataDeclarationWithConstructors original newVal = Ast.DataDeclaration {
  Ast.dataDeclarationKeyword = (Ast.dataDeclarationKeyword original),
  Ast.dataDeclarationContext = (Ast.dataDeclarationContext original),
  Ast.dataDeclarationHead = (Ast.dataDeclarationHead original),
  Ast.dataDeclarationConstructors = newVal,
  Ast.dataDeclarationDeriving = (Ast.dataDeclarationDeriving original)}

dataDeclarationWithDeriving :: (Ast.DataDeclaration -> [Ast.Deriving] -> Ast.DataDeclaration)
dataDeclarationWithDeriving original newVal = Ast.DataDeclaration {
  Ast.dataDeclarationKeyword = (Ast.dataDeclarationKeyword original),
  Ast.dataDeclarationContext = (Ast.dataDeclarationContext original),
  Ast.dataDeclarationHead = (Ast.dataDeclarationHead original),
  Ast.dataDeclarationConstructors = (Ast.dataDeclarationConstructors original),
  Ast.dataDeclarationDeriving = newVal}

dataOrNewtypeData :: Ast.DataOrNewtype
dataOrNewtypeData = Ast.DataOrNewtypeData

dataOrNewtypeNewtype :: Ast.DataOrNewtype
dataOrNewtypeNewtype = Ast.DataOrNewtypeNewtype

declarationWithComments :: (Ast.Declaration -> Maybe String -> Ast.DeclarationWithComments)
declarationWithComments body comments = Ast.DeclarationWithComments {
  Ast.declarationWithCommentsBody = body,
  Ast.declarationWithCommentsComments = comments}

declarationWithCommentsBody :: (Ast.DeclarationWithComments -> Ast.Declaration)
declarationWithCommentsBody = Ast.declarationWithCommentsBody

declarationWithCommentsComments :: (Ast.DeclarationWithComments -> Maybe String)
declarationWithCommentsComments = Ast.declarationWithCommentsComments

declarationWithCommentsWithBody :: (Ast.DeclarationWithComments -> Ast.Declaration -> Ast.DeclarationWithComments)
declarationWithCommentsWithBody original newVal = Ast.DeclarationWithComments {
  Ast.declarationWithCommentsBody = newVal,
  Ast.declarationWithCommentsComments = (Ast.declarationWithCommentsComments original)}

declarationWithCommentsWithComments :: (Ast.DeclarationWithComments -> Maybe String -> Ast.DeclarationWithComments)
declarationWithCommentsWithComments original newVal = Ast.DeclarationWithComments {
  Ast.declarationWithCommentsBody = (Ast.declarationWithCommentsBody original),
  Ast.declarationWithCommentsComments = newVal}

declarationData :: (Ast.DataDeclaration -> Ast.Declaration)
declarationData x = (Ast.DeclarationData x)

declarationType :: (Ast.TypeDeclaration -> Ast.Declaration)
declarationType x = (Ast.DeclarationType x)

declarationValueBinding :: (Ast.ValueBinding -> Ast.Declaration)
declarationValueBinding x = (Ast.DeclarationValueBinding x)

declarationTypedBinding :: (Ast.TypedBinding -> Ast.Declaration)
declarationTypedBinding x = (Ast.DeclarationTypedBinding x)

declarationHeadApplication :: (Ast.ApplicationDeclarationHead -> Ast.DeclarationHead)
declarationHeadApplication x = (Ast.DeclarationHeadApplication x)

declarationHeadParens :: (Ast.DeclarationHead -> Ast.DeclarationHead)
declarationHeadParens x = (Ast.DeclarationHeadParens x)

declarationHeadSimple :: (Ast.Name -> Ast.DeclarationHead)
declarationHeadSimple x = (Ast.DeclarationHeadSimple x)

applicationDeclarationHead :: (Ast.DeclarationHead -> Ast.Variable -> Ast.ApplicationDeclarationHead)
applicationDeclarationHead function operand = Ast.ApplicationDeclarationHead {
  Ast.applicationDeclarationHeadFunction = function,
  Ast.applicationDeclarationHeadOperand = operand}

applicationDeclarationHeadFunction :: (Ast.ApplicationDeclarationHead -> Ast.DeclarationHead)
applicationDeclarationHeadFunction = Ast.applicationDeclarationHeadFunction

applicationDeclarationHeadOperand :: (Ast.ApplicationDeclarationHead -> Ast.Variable)
applicationDeclarationHeadOperand = Ast.applicationDeclarationHeadOperand

applicationDeclarationHeadWithFunction :: (Ast.ApplicationDeclarationHead -> Ast.DeclarationHead -> Ast.ApplicationDeclarationHead)
applicationDeclarationHeadWithFunction original newVal = Ast.ApplicationDeclarationHead {
  Ast.applicationDeclarationHeadFunction = newVal,
  Ast.applicationDeclarationHeadOperand = (Ast.applicationDeclarationHeadOperand original)}

applicationDeclarationHeadWithOperand :: (Ast.ApplicationDeclarationHead -> Ast.Variable -> Ast.ApplicationDeclarationHead)
applicationDeclarationHeadWithOperand original newVal = Ast.ApplicationDeclarationHead {
  Ast.applicationDeclarationHeadFunction = (Ast.applicationDeclarationHeadFunction original),
  Ast.applicationDeclarationHeadOperand = newVal}

deriving_ :: ([Ast.Name] -> Ast.Deriving)
deriving_ x = (Ast.Deriving x)

unDeriving :: (Ast.Deriving -> [Ast.Name])
unDeriving = Ast.unDeriving

exportDeclaration :: (Ast.ImportExportSpec -> Ast.Export)
exportDeclaration x = (Ast.ExportDeclaration x)

exportModule :: (Ast.ModuleName -> Ast.Export)
exportModule x = (Ast.ExportModule x)

expressionApplication :: (Ast.ApplicationExpression -> Ast.Expression)
expressionApplication x = (Ast.ExpressionApplication x)

expressionCase :: (Ast.CaseExpression -> Ast.Expression)
expressionCase x = (Ast.ExpressionCase x)

expressionConstructRecord :: (Ast.ConstructRecordExpression -> Ast.Expression)
expressionConstructRecord x = (Ast.ExpressionConstructRecord x)

expressionDo :: ([Ast.Statement] -> Ast.Expression)
expressionDo x = (Ast.ExpressionDo x)

expressionIf :: (Ast.IfExpression -> Ast.Expression)
expressionIf x = (Ast.ExpressionIf x)

expressionInfixApplication :: (Ast.InfixApplicationExpression -> Ast.Expression)
expressionInfixApplication x = (Ast.ExpressionInfixApplication x)

expressionLiteral :: (Ast.Literal -> Ast.Expression)
expressionLiteral x = (Ast.ExpressionLiteral x)

expressionLambda :: (Ast.LambdaExpression -> Ast.Expression)
expressionLambda x = (Ast.ExpressionLambda x)

expressionLeftSection :: (Ast.SectionExpression -> Ast.Expression)
expressionLeftSection x = (Ast.ExpressionLeftSection x)

expressionLet :: (Ast.LetExpression -> Ast.Expression)
expressionLet x = (Ast.ExpressionLet x)

expressionList :: ([Ast.Expression] -> Ast.Expression)
expressionList x = (Ast.ExpressionList x)

expressionParens :: (Ast.Expression -> Ast.Expression)
expressionParens x = (Ast.ExpressionParens x)

expressionPrefixApplication :: (Ast.PrefixApplicationExpression -> Ast.Expression)
expressionPrefixApplication x = (Ast.ExpressionPrefixApplication x)

expressionRightSection :: (Ast.SectionExpression -> Ast.Expression)
expressionRightSection x = (Ast.ExpressionRightSection x)

expressionTuple :: ([Ast.Expression] -> Ast.Expression)
expressionTuple x = (Ast.ExpressionTuple x)

expressionTypeSignature :: (Ast.TypeSignatureExpression -> Ast.Expression)
expressionTypeSignature x = (Ast.ExpressionTypeSignature x)

expressionUpdateRecord :: (Ast.UpdateRecordExpression -> Ast.Expression)
expressionUpdateRecord x = (Ast.ExpressionUpdateRecord x)

expressionVariable :: (Ast.Name -> Ast.Expression)
expressionVariable x = (Ast.ExpressionVariable x)

applicationExpression :: (Ast.Expression -> Ast.Expression -> Ast.ApplicationExpression)
applicationExpression function argument = Ast.ApplicationExpression {
  Ast.applicationExpressionFunction = function,
  Ast.applicationExpressionArgument = argument}

applicationExpressionFunction :: (Ast.ApplicationExpression -> Ast.Expression)
applicationExpressionFunction = Ast.applicationExpressionFunction

applicationExpressionArgument :: (Ast.ApplicationExpression -> Ast.Expression)
applicationExpressionArgument = Ast.applicationExpressionArgument

applicationExpressionWithFunction :: (Ast.ApplicationExpression -> Ast.Expression -> Ast.ApplicationExpression)
applicationExpressionWithFunction original newVal = Ast.ApplicationExpression {
  Ast.applicationExpressionFunction = newVal,
  Ast.applicationExpressionArgument = (Ast.applicationExpressionArgument original)}

applicationExpressionWithArgument :: (Ast.ApplicationExpression -> Ast.Expression -> Ast.ApplicationExpression)
applicationExpressionWithArgument original newVal = Ast.ApplicationExpression {
  Ast.applicationExpressionFunction = (Ast.applicationExpressionFunction original),
  Ast.applicationExpressionArgument = newVal}

caseExpression :: (Ast.Expression -> [Ast.Alternative] -> Ast.CaseExpression)
caseExpression case_ alternatives = Ast.CaseExpression {
  Ast.caseExpressionCase = case_,
  Ast.caseExpressionAlternatives = alternatives}

caseExpressionCase :: (Ast.CaseExpression -> Ast.Expression)
caseExpressionCase = Ast.caseExpressionCase

caseExpressionAlternatives :: (Ast.CaseExpression -> [Ast.Alternative])
caseExpressionAlternatives = Ast.caseExpressionAlternatives

caseExpressionWithCase :: (Ast.CaseExpression -> Ast.Expression -> Ast.CaseExpression)
caseExpressionWithCase original newVal = Ast.CaseExpression {
  Ast.caseExpressionCase = newVal,
  Ast.caseExpressionAlternatives = (Ast.caseExpressionAlternatives original)}

caseExpressionWithAlternatives :: (Ast.CaseExpression -> [Ast.Alternative] -> Ast.CaseExpression)
caseExpressionWithAlternatives original newVal = Ast.CaseExpression {
  Ast.caseExpressionCase = (Ast.caseExpressionCase original),
  Ast.caseExpressionAlternatives = newVal}

constructRecordExpression :: (Ast.Name -> [Ast.FieldUpdate] -> Ast.ConstructRecordExpression)
constructRecordExpression name fields = Ast.ConstructRecordExpression {
  Ast.constructRecordExpressionName = name,
  Ast.constructRecordExpressionFields = fields}

constructRecordExpressionName :: (Ast.ConstructRecordExpression -> Ast.Name)
constructRecordExpressionName = Ast.constructRecordExpressionName

constructRecordExpressionFields :: (Ast.ConstructRecordExpression -> [Ast.FieldUpdate])
constructRecordExpressionFields = Ast.constructRecordExpressionFields

constructRecordExpressionWithName :: (Ast.ConstructRecordExpression -> Ast.Name -> Ast.ConstructRecordExpression)
constructRecordExpressionWithName original newVal = Ast.ConstructRecordExpression {
  Ast.constructRecordExpressionName = newVal,
  Ast.constructRecordExpressionFields = (Ast.constructRecordExpressionFields original)}

constructRecordExpressionWithFields :: (Ast.ConstructRecordExpression -> [Ast.FieldUpdate] -> Ast.ConstructRecordExpression)
constructRecordExpressionWithFields original newVal = Ast.ConstructRecordExpression {
  Ast.constructRecordExpressionName = (Ast.constructRecordExpressionName original),
  Ast.constructRecordExpressionFields = newVal}

ifExpression :: (Ast.Expression -> Ast.Expression -> Ast.Expression -> Ast.IfExpression)
ifExpression condition then_ else_ = Ast.IfExpression {
  Ast.ifExpressionCondition = condition,
  Ast.ifExpressionThen = then_,
  Ast.ifExpressionElse = else_}

ifExpressionCondition :: (Ast.IfExpression -> Ast.Expression)
ifExpressionCondition = Ast.ifExpressionCondition

ifExpressionThen :: (Ast.IfExpression -> Ast.Expression)
ifExpressionThen = Ast.ifExpressionThen

ifExpressionElse :: (Ast.IfExpression -> Ast.Expression)
ifExpressionElse = Ast.ifExpressionElse

ifExpressionWithCondition :: (Ast.IfExpression -> Ast.Expression -> Ast.IfExpression)
ifExpressionWithCondition original newVal = Ast.IfExpression {
  Ast.ifExpressionCondition = newVal,
  Ast.ifExpressionThen = (Ast.ifExpressionThen original),
  Ast.ifExpressionElse = (Ast.ifExpressionElse original)}

ifExpressionWithThen :: (Ast.IfExpression -> Ast.Expression -> Ast.IfExpression)
ifExpressionWithThen original newVal = Ast.IfExpression {
  Ast.ifExpressionCondition = (Ast.ifExpressionCondition original),
  Ast.ifExpressionThen = newVal,
  Ast.ifExpressionElse = (Ast.ifExpressionElse original)}

ifExpressionWithElse :: (Ast.IfExpression -> Ast.Expression -> Ast.IfExpression)
ifExpressionWithElse original newVal = Ast.IfExpression {
  Ast.ifExpressionCondition = (Ast.ifExpressionCondition original),
  Ast.ifExpressionThen = (Ast.ifExpressionThen original),
  Ast.ifExpressionElse = newVal}

infixApplicationExpression :: (Ast.Expression -> Ast.Operator -> Ast.Expression -> Ast.InfixApplicationExpression)
infixApplicationExpression lhs operator rhs = Ast.InfixApplicationExpression {
  Ast.infixApplicationExpressionLhs = lhs,
  Ast.infixApplicationExpressionOperator = operator,
  Ast.infixApplicationExpressionRhs = rhs}

infixApplicationExpressionLhs :: (Ast.InfixApplicationExpression -> Ast.Expression)
infixApplicationExpressionLhs = Ast.infixApplicationExpressionLhs

infixApplicationExpressionOperator :: (Ast.InfixApplicationExpression -> Ast.Operator)
infixApplicationExpressionOperator = Ast.infixApplicationExpressionOperator

infixApplicationExpressionRhs :: (Ast.InfixApplicationExpression -> Ast.Expression)
infixApplicationExpressionRhs = Ast.infixApplicationExpressionRhs

infixApplicationExpressionWithLhs :: (Ast.InfixApplicationExpression -> Ast.Expression -> Ast.InfixApplicationExpression)
infixApplicationExpressionWithLhs original newVal = Ast.InfixApplicationExpression {
  Ast.infixApplicationExpressionLhs = newVal,
  Ast.infixApplicationExpressionOperator = (Ast.infixApplicationExpressionOperator original),
  Ast.infixApplicationExpressionRhs = (Ast.infixApplicationExpressionRhs original)}

infixApplicationExpressionWithOperator :: (Ast.InfixApplicationExpression -> Ast.Operator -> Ast.InfixApplicationExpression)
infixApplicationExpressionWithOperator original newVal = Ast.InfixApplicationExpression {
  Ast.infixApplicationExpressionLhs = (Ast.infixApplicationExpressionLhs original),
  Ast.infixApplicationExpressionOperator = newVal,
  Ast.infixApplicationExpressionRhs = (Ast.infixApplicationExpressionRhs original)}

infixApplicationExpressionWithRhs :: (Ast.InfixApplicationExpression -> Ast.Expression -> Ast.InfixApplicationExpression)
infixApplicationExpressionWithRhs original newVal = Ast.InfixApplicationExpression {
  Ast.infixApplicationExpressionLhs = (Ast.infixApplicationExpressionLhs original),
  Ast.infixApplicationExpressionOperator = (Ast.infixApplicationExpressionOperator original),
  Ast.infixApplicationExpressionRhs = newVal}

lambdaExpression :: ([Ast.Pattern] -> Ast.Expression -> Ast.LambdaExpression)
lambdaExpression bindings inner = Ast.LambdaExpression {
  Ast.lambdaExpressionBindings = bindings,
  Ast.lambdaExpressionInner = inner}

lambdaExpressionBindings :: (Ast.LambdaExpression -> [Ast.Pattern])
lambdaExpressionBindings = Ast.lambdaExpressionBindings

lambdaExpressionInner :: (Ast.LambdaExpression -> Ast.Expression)
lambdaExpressionInner = Ast.lambdaExpressionInner

lambdaExpressionWithBindings :: (Ast.LambdaExpression -> [Ast.Pattern] -> Ast.LambdaExpression)
lambdaExpressionWithBindings original newVal = Ast.LambdaExpression {
  Ast.lambdaExpressionBindings = newVal,
  Ast.lambdaExpressionInner = (Ast.lambdaExpressionInner original)}

lambdaExpressionWithInner :: (Ast.LambdaExpression -> Ast.Expression -> Ast.LambdaExpression)
lambdaExpressionWithInner original newVal = Ast.LambdaExpression {
  Ast.lambdaExpressionBindings = (Ast.lambdaExpressionBindings original),
  Ast.lambdaExpressionInner = newVal}

letExpression :: ([Ast.LocalBinding] -> Ast.Expression -> Ast.LetExpression)
letExpression bindings inner = Ast.LetExpression {
  Ast.letExpressionBindings = bindings,
  Ast.letExpressionInner = inner}

letExpressionBindings :: (Ast.LetExpression -> [Ast.LocalBinding])
letExpressionBindings = Ast.letExpressionBindings

letExpressionInner :: (Ast.LetExpression -> Ast.Expression)
letExpressionInner = Ast.letExpressionInner

letExpressionWithBindings :: (Ast.LetExpression -> [Ast.LocalBinding] -> Ast.LetExpression)
letExpressionWithBindings original newVal = Ast.LetExpression {
  Ast.letExpressionBindings = newVal,
  Ast.letExpressionInner = (Ast.letExpressionInner original)}

letExpressionWithInner :: (Ast.LetExpression -> Ast.Expression -> Ast.LetExpression)
letExpressionWithInner original newVal = Ast.LetExpression {
  Ast.letExpressionBindings = (Ast.letExpressionBindings original),
  Ast.letExpressionInner = newVal}

prefixApplicationExpression :: (Ast.Operator -> Ast.Expression -> Ast.PrefixApplicationExpression)
prefixApplicationExpression operator rhs = Ast.PrefixApplicationExpression {
  Ast.prefixApplicationExpressionOperator = operator,
  Ast.prefixApplicationExpressionRhs = rhs}

prefixApplicationExpressionOperator :: (Ast.PrefixApplicationExpression -> Ast.Operator)
prefixApplicationExpressionOperator = Ast.prefixApplicationExpressionOperator

prefixApplicationExpressionRhs :: (Ast.PrefixApplicationExpression -> Ast.Expression)
prefixApplicationExpressionRhs = Ast.prefixApplicationExpressionRhs

prefixApplicationExpressionWithOperator :: (Ast.PrefixApplicationExpression -> Ast.Operator -> Ast.PrefixApplicationExpression)
prefixApplicationExpressionWithOperator original newVal = Ast.PrefixApplicationExpression {
  Ast.prefixApplicationExpressionOperator = newVal,
  Ast.prefixApplicationExpressionRhs = (Ast.prefixApplicationExpressionRhs original)}

prefixApplicationExpressionWithRhs :: (Ast.PrefixApplicationExpression -> Ast.Expression -> Ast.PrefixApplicationExpression)
prefixApplicationExpressionWithRhs original newVal = Ast.PrefixApplicationExpression {
  Ast.prefixApplicationExpressionOperator = (Ast.prefixApplicationExpressionOperator original),
  Ast.prefixApplicationExpressionRhs = newVal}

sectionExpression :: (Ast.Operator -> Ast.Expression -> Ast.SectionExpression)
sectionExpression operator expression = Ast.SectionExpression {
  Ast.sectionExpressionOperator = operator,
  Ast.sectionExpressionExpression = expression}

sectionExpressionOperator :: (Ast.SectionExpression -> Ast.Operator)
sectionExpressionOperator = Ast.sectionExpressionOperator

sectionExpressionExpression :: (Ast.SectionExpression -> Ast.Expression)
sectionExpressionExpression = Ast.sectionExpressionExpression

sectionExpressionWithOperator :: (Ast.SectionExpression -> Ast.Operator -> Ast.SectionExpression)
sectionExpressionWithOperator original newVal = Ast.SectionExpression {
  Ast.sectionExpressionOperator = newVal,
  Ast.sectionExpressionExpression = (Ast.sectionExpressionExpression original)}

sectionExpressionWithExpression :: (Ast.SectionExpression -> Ast.Expression -> Ast.SectionExpression)
sectionExpressionWithExpression original newVal = Ast.SectionExpression {
  Ast.sectionExpressionOperator = (Ast.sectionExpressionOperator original),
  Ast.sectionExpressionExpression = newVal}

typeSignatureExpression :: (Ast.Expression -> Ast.Type -> Ast.TypeSignatureExpression)
typeSignatureExpression inner type_ = Ast.TypeSignatureExpression {
  Ast.typeSignatureExpressionInner = inner,
  Ast.typeSignatureExpressionType = type_}

typeSignatureExpressionInner :: (Ast.TypeSignatureExpression -> Ast.Expression)
typeSignatureExpressionInner = Ast.typeSignatureExpressionInner

typeSignatureExpressionType :: (Ast.TypeSignatureExpression -> Ast.Type)
typeSignatureExpressionType = Ast.typeSignatureExpressionType

typeSignatureExpressionWithInner :: (Ast.TypeSignatureExpression -> Ast.Expression -> Ast.TypeSignatureExpression)
typeSignatureExpressionWithInner original newVal = Ast.TypeSignatureExpression {
  Ast.typeSignatureExpressionInner = newVal,
  Ast.typeSignatureExpressionType = (Ast.typeSignatureExpressionType original)}

typeSignatureExpressionWithType :: (Ast.TypeSignatureExpression -> Ast.Type -> Ast.TypeSignatureExpression)
typeSignatureExpressionWithType original newVal = Ast.TypeSignatureExpression {
  Ast.typeSignatureExpressionInner = (Ast.typeSignatureExpressionInner original),
  Ast.typeSignatureExpressionType = newVal}

updateRecordExpression :: (Ast.Expression -> [Ast.FieldUpdate] -> Ast.UpdateRecordExpression)
updateRecordExpression inner fields = Ast.UpdateRecordExpression {
  Ast.updateRecordExpressionInner = inner,
  Ast.updateRecordExpressionFields = fields}

updateRecordExpressionInner :: (Ast.UpdateRecordExpression -> Ast.Expression)
updateRecordExpressionInner = Ast.updateRecordExpressionInner

updateRecordExpressionFields :: (Ast.UpdateRecordExpression -> [Ast.FieldUpdate])
updateRecordExpressionFields = Ast.updateRecordExpressionFields

updateRecordExpressionWithInner :: (Ast.UpdateRecordExpression -> Ast.Expression -> Ast.UpdateRecordExpression)
updateRecordExpressionWithInner original newVal = Ast.UpdateRecordExpression {
  Ast.updateRecordExpressionInner = newVal,
  Ast.updateRecordExpressionFields = (Ast.updateRecordExpressionFields original)}

updateRecordExpressionWithFields :: (Ast.UpdateRecordExpression -> [Ast.FieldUpdate] -> Ast.UpdateRecordExpression)
updateRecordExpressionWithFields original newVal = Ast.UpdateRecordExpression {
  Ast.updateRecordExpressionInner = (Ast.updateRecordExpressionInner original),
  Ast.updateRecordExpressionFields = newVal}

field :: (Ast.Name -> Ast.Type -> Ast.Field)
field name type_ = Ast.Field {
  Ast.fieldName = name,
  Ast.fieldType = type_}

fieldName :: (Ast.Field -> Ast.Name)
fieldName = Ast.fieldName

fieldType :: (Ast.Field -> Ast.Type)
fieldType = Ast.fieldType

fieldWithName :: (Ast.Field -> Ast.Name -> Ast.Field)
fieldWithName original newVal = Ast.Field {
  Ast.fieldName = newVal,
  Ast.fieldType = (Ast.fieldType original)}

fieldWithType :: (Ast.Field -> Ast.Type -> Ast.Field)
fieldWithType original newVal = Ast.Field {
  Ast.fieldName = (Ast.fieldName original),
  Ast.fieldType = newVal}

fieldWithComments :: (Ast.Field -> Maybe String -> Ast.FieldWithComments)
fieldWithComments field comments = Ast.FieldWithComments {
  Ast.fieldWithCommentsField = field,
  Ast.fieldWithCommentsComments = comments}

fieldWithCommentsField :: (Ast.FieldWithComments -> Ast.Field)
fieldWithCommentsField = Ast.fieldWithCommentsField

fieldWithCommentsComments :: (Ast.FieldWithComments -> Maybe String)
fieldWithCommentsComments = Ast.fieldWithCommentsComments

fieldWithCommentsWithField :: (Ast.FieldWithComments -> Ast.Field -> Ast.FieldWithComments)
fieldWithCommentsWithField original newVal = Ast.FieldWithComments {
  Ast.fieldWithCommentsField = newVal,
  Ast.fieldWithCommentsComments = (Ast.fieldWithCommentsComments original)}

fieldWithCommentsWithComments :: (Ast.FieldWithComments -> Maybe String -> Ast.FieldWithComments)
fieldWithCommentsWithComments original newVal = Ast.FieldWithComments {
  Ast.fieldWithCommentsField = (Ast.fieldWithCommentsField original),
  Ast.fieldWithCommentsComments = newVal}

fieldUpdate :: (Ast.Name -> Ast.Expression -> Ast.FieldUpdate)
fieldUpdate name value = Ast.FieldUpdate {
  Ast.fieldUpdateName = name,
  Ast.fieldUpdateValue = value}

fieldUpdateName :: (Ast.FieldUpdate -> Ast.Name)
fieldUpdateName = Ast.fieldUpdateName

fieldUpdateValue :: (Ast.FieldUpdate -> Ast.Expression)
fieldUpdateValue = Ast.fieldUpdateValue

fieldUpdateWithName :: (Ast.FieldUpdate -> Ast.Name -> Ast.FieldUpdate)
fieldUpdateWithName original newVal = Ast.FieldUpdate {
  Ast.fieldUpdateName = newVal,
  Ast.fieldUpdateValue = (Ast.fieldUpdateValue original)}

fieldUpdateWithValue :: (Ast.FieldUpdate -> Ast.Expression -> Ast.FieldUpdate)
fieldUpdateWithValue original newVal = Ast.FieldUpdate {
  Ast.fieldUpdateName = (Ast.fieldUpdateName original),
  Ast.fieldUpdateValue = newVal}

import_ :: (Bool -> Ast.ModuleName -> Maybe Ast.ModuleName -> Maybe Ast.SpecImport -> Ast.Import)
import_ qualified module_ as spec = Ast.Import {
  Ast.importQualified = qualified,
  Ast.importModule = module_,
  Ast.importAs = as,
  Ast.importSpec = spec}

importQualified :: (Ast.Import -> Bool)
importQualified = Ast.importQualified

importModule :: (Ast.Import -> Ast.ModuleName)
importModule = Ast.importModule

importAs :: (Ast.Import -> Maybe Ast.ModuleName)
importAs = Ast.importAs

importSpec :: (Ast.Import -> Maybe Ast.SpecImport)
importSpec = Ast.importSpec

importWithQualified :: (Ast.Import -> Bool -> Ast.Import)
importWithQualified original newVal = Ast.Import {
  Ast.importQualified = newVal,
  Ast.importModule = (Ast.importModule original),
  Ast.importAs = (Ast.importAs original),
  Ast.importSpec = (Ast.importSpec original)}

importWithModule :: (Ast.Import -> Ast.ModuleName -> Ast.Import)
importWithModule original newVal = Ast.Import {
  Ast.importQualified = (Ast.importQualified original),
  Ast.importModule = newVal,
  Ast.importAs = (Ast.importAs original),
  Ast.importSpec = (Ast.importSpec original)}

importWithAs :: (Ast.Import -> Maybe Ast.ModuleName -> Ast.Import)
importWithAs original newVal = Ast.Import {
  Ast.importQualified = (Ast.importQualified original),
  Ast.importModule = (Ast.importModule original),
  Ast.importAs = newVal,
  Ast.importSpec = (Ast.importSpec original)}

importWithSpec :: (Ast.Import -> Maybe Ast.SpecImport -> Ast.Import)
importWithSpec original newVal = Ast.Import {
  Ast.importQualified = (Ast.importQualified original),
  Ast.importModule = (Ast.importModule original),
  Ast.importAs = (Ast.importAs original),
  Ast.importSpec = newVal}

specImportList :: ([Ast.ImportExportSpec] -> Ast.SpecImport)
specImportList x = (Ast.SpecImportList x)

specImportHiding :: ([Ast.ImportExportSpec] -> Ast.SpecImport)
specImportHiding x = (Ast.SpecImportHiding x)

importModifierPattern :: Ast.ImportModifier
importModifierPattern = Ast.ImportModifierPattern

importModifierType :: Ast.ImportModifier
importModifierType = Ast.ImportModifierType

importExportSpec :: (Maybe Ast.ImportModifier -> Ast.Name -> Maybe Ast.SubspecImportExportSpec -> Ast.ImportExportSpec)
importExportSpec modifier name subspec = Ast.ImportExportSpec {
  Ast.importExportSpecModifier = modifier,
  Ast.importExportSpecName = name,
  Ast.importExportSpecSubspec = subspec}

importExportSpecModifier :: (Ast.ImportExportSpec -> Maybe Ast.ImportModifier)
importExportSpecModifier = Ast.importExportSpecModifier

importExportSpecName :: (Ast.ImportExportSpec -> Ast.Name)
importExportSpecName = Ast.importExportSpecName

importExportSpecSubspec :: (Ast.ImportExportSpec -> Maybe Ast.SubspecImportExportSpec)
importExportSpecSubspec = Ast.importExportSpecSubspec

importExportSpecWithModifier :: (Ast.ImportExportSpec -> Maybe Ast.ImportModifier -> Ast.ImportExportSpec)
importExportSpecWithModifier original newVal = Ast.ImportExportSpec {
  Ast.importExportSpecModifier = newVal,
  Ast.importExportSpecName = (Ast.importExportSpecName original),
  Ast.importExportSpecSubspec = (Ast.importExportSpecSubspec original)}

importExportSpecWithName :: (Ast.ImportExportSpec -> Ast.Name -> Ast.ImportExportSpec)
importExportSpecWithName original newVal = Ast.ImportExportSpec {
  Ast.importExportSpecModifier = (Ast.importExportSpecModifier original),
  Ast.importExportSpecName = newVal,
  Ast.importExportSpecSubspec = (Ast.importExportSpecSubspec original)}

importExportSpecWithSubspec :: (Ast.ImportExportSpec -> Maybe Ast.SubspecImportExportSpec -> Ast.ImportExportSpec)
importExportSpecWithSubspec original newVal = Ast.ImportExportSpec {
  Ast.importExportSpecModifier = (Ast.importExportSpecModifier original),
  Ast.importExportSpecName = (Ast.importExportSpecName original),
  Ast.importExportSpecSubspec = newVal}

subspecImportExportSpecAll :: Ast.SubspecImportExportSpec
subspecImportExportSpecAll = Ast.SubspecImportExportSpecAll

subspecImportExportSpecList :: ([Ast.Name] -> Ast.SubspecImportExportSpec)
subspecImportExportSpecList x = (Ast.SubspecImportExportSpecList x)

literalChar :: (Int -> Ast.Literal)
literalChar x = (Ast.LiteralChar x)

literalDouble :: (Double -> Ast.Literal)
literalDouble x = (Ast.LiteralDouble x)

literalFloat :: (Float -> Ast.Literal)
literalFloat x = (Ast.LiteralFloat x)

literalInt :: (Int -> Ast.Literal)
literalInt x = (Ast.LiteralInt x)

literalInteger :: (Integer -> Ast.Literal)
literalInteger x = (Ast.LiteralInteger x)

literalString :: (String -> Ast.Literal)
literalString x = (Ast.LiteralString x)

localBindingSignature :: (Ast.TypeSignature -> Ast.LocalBinding)
localBindingSignature x = (Ast.LocalBindingSignature x)

localBindingValue :: (Ast.ValueBinding -> Ast.LocalBinding)
localBindingValue x = (Ast.LocalBindingValue x)

localBindings :: ([Ast.LocalBinding] -> Ast.LocalBindings)
localBindings x = (Ast.LocalBindings x)

unLocalBindings :: (Ast.LocalBindings -> [Ast.LocalBinding])
unLocalBindings = Ast.unLocalBindings

module_ :: (Maybe Ast.ModuleHead -> [Ast.Import] -> [Ast.DeclarationWithComments] -> Ast.Module)
module_ head imports declarations = Ast.Module {
  Ast.moduleHead = head,
  Ast.moduleImports = imports,
  Ast.moduleDeclarations = declarations}

moduleHead :: (Ast.Module -> Maybe Ast.ModuleHead)
moduleHead = Ast.moduleHead

moduleImports :: (Ast.Module -> [Ast.Import])
moduleImports = Ast.moduleImports

moduleDeclarations :: (Ast.Module -> [Ast.DeclarationWithComments])
moduleDeclarations = Ast.moduleDeclarations

moduleWithHead :: (Ast.Module -> Maybe Ast.ModuleHead -> Ast.Module)
moduleWithHead original newVal = Ast.Module {
  Ast.moduleHead = newVal,
  Ast.moduleImports = (Ast.moduleImports original),
  Ast.moduleDeclarations = (Ast.moduleDeclarations original)}

moduleWithImports :: (Ast.Module -> [Ast.Import] -> Ast.Module)
moduleWithImports original newVal = Ast.Module {
  Ast.moduleHead = (Ast.moduleHead original),
  Ast.moduleImports = newVal,
  Ast.moduleDeclarations = (Ast.moduleDeclarations original)}

moduleWithDeclarations :: (Ast.Module -> [Ast.DeclarationWithComments] -> Ast.Module)
moduleWithDeclarations original newVal = Ast.Module {
  Ast.moduleHead = (Ast.moduleHead original),
  Ast.moduleImports = (Ast.moduleImports original),
  Ast.moduleDeclarations = newVal}

moduleHead_ :: (Maybe String -> Ast.ModuleName -> [Ast.Export] -> Ast.ModuleHead)
moduleHead_ comments name exports = Ast.ModuleHead {
  Ast.moduleHeadComments = comments,
  Ast.moduleHeadName = name,
  Ast.moduleHeadExports = exports}

moduleHeadComments :: (Ast.ModuleHead -> Maybe String)
moduleHeadComments = Ast.moduleHeadComments

moduleHeadName :: (Ast.ModuleHead -> Ast.ModuleName)
moduleHeadName = Ast.moduleHeadName

moduleHeadExports :: (Ast.ModuleHead -> [Ast.Export])
moduleHeadExports = Ast.moduleHeadExports

moduleHeadWithComments :: (Ast.ModuleHead -> Maybe String -> Ast.ModuleHead)
moduleHeadWithComments original newVal = Ast.ModuleHead {
  Ast.moduleHeadComments = newVal,
  Ast.moduleHeadName = (Ast.moduleHeadName original),
  Ast.moduleHeadExports = (Ast.moduleHeadExports original)}

moduleHeadWithName :: (Ast.ModuleHead -> Ast.ModuleName -> Ast.ModuleHead)
moduleHeadWithName original newVal = Ast.ModuleHead {
  Ast.moduleHeadComments = (Ast.moduleHeadComments original),
  Ast.moduleHeadName = newVal,
  Ast.moduleHeadExports = (Ast.moduleHeadExports original)}

moduleHeadWithExports :: (Ast.ModuleHead -> [Ast.Export] -> Ast.ModuleHead)
moduleHeadWithExports original newVal = Ast.ModuleHead {
  Ast.moduleHeadComments = (Ast.moduleHeadComments original),
  Ast.moduleHeadName = (Ast.moduleHeadName original),
  Ast.moduleHeadExports = newVal}

moduleName :: (String -> Ast.ModuleName)
moduleName x = (Ast.ModuleName x)

unModuleName :: (Ast.ModuleName -> String)
unModuleName = Ast.unModuleName

nameImplicit :: (Ast.QualifiedName -> Ast.Name)
nameImplicit x = (Ast.NameImplicit x)

nameNormal :: (Ast.QualifiedName -> Ast.Name)
nameNormal x = (Ast.NameNormal x)

nameParens :: (Ast.QualifiedName -> Ast.Name)
nameParens x = (Ast.NameParens x)

namePart :: (String -> Ast.NamePart)
namePart x = (Ast.NamePart x)

unNamePart :: (Ast.NamePart -> String)
unNamePart = Ast.unNamePart

operatorBacktick :: (Ast.QualifiedName -> Ast.Operator)
operatorBacktick x = (Ast.OperatorBacktick x)

operatorNormal :: (Ast.QualifiedName -> Ast.Operator)
operatorNormal x = (Ast.OperatorNormal x)

patternApplication :: (Ast.ApplicationPattern -> Ast.Pattern)
patternApplication x = (Ast.PatternApplication x)

patternAs :: (Ast.AsPattern -> Ast.Pattern)
patternAs x = (Ast.PatternAs x)

patternList :: ([Ast.Pattern] -> Ast.Pattern)
patternList x = (Ast.PatternList x)

patternLiteral :: (Ast.Literal -> Ast.Pattern)
patternLiteral x = (Ast.PatternLiteral x)

patternName :: (Ast.Name -> Ast.Pattern)
patternName x = (Ast.PatternName x)

patternParens :: (Ast.Pattern -> Ast.Pattern)
patternParens x = (Ast.PatternParens x)

patternRecord :: (Ast.RecordPattern -> Ast.Pattern)
patternRecord x = (Ast.PatternRecord x)

patternTuple :: ([Ast.Pattern] -> Ast.Pattern)
patternTuple x = (Ast.PatternTuple x)

patternTyped :: (Ast.TypedPattern -> Ast.Pattern)
patternTyped x = (Ast.PatternTyped x)

patternWildcard :: Ast.Pattern
patternWildcard = Ast.PatternWildcard

applicationPattern :: (Ast.Name -> [Ast.Pattern] -> Ast.ApplicationPattern)
applicationPattern name args = Ast.ApplicationPattern {
  Ast.applicationPatternName = name,
  Ast.applicationPatternArgs = args}

applicationPatternName :: (Ast.ApplicationPattern -> Ast.Name)
applicationPatternName = Ast.applicationPatternName

applicationPatternArgs :: (Ast.ApplicationPattern -> [Ast.Pattern])
applicationPatternArgs = Ast.applicationPatternArgs

applicationPatternWithName :: (Ast.ApplicationPattern -> Ast.Name -> Ast.ApplicationPattern)
applicationPatternWithName original newVal = Ast.ApplicationPattern {
  Ast.applicationPatternName = newVal,
  Ast.applicationPatternArgs = (Ast.applicationPatternArgs original)}

applicationPatternWithArgs :: (Ast.ApplicationPattern -> [Ast.Pattern] -> Ast.ApplicationPattern)
applicationPatternWithArgs original newVal = Ast.ApplicationPattern {
  Ast.applicationPatternName = (Ast.applicationPatternName original),
  Ast.applicationPatternArgs = newVal}

asPattern :: (Ast.Name -> Ast.Pattern -> Ast.AsPattern)
asPattern name inner = Ast.AsPattern {
  Ast.asPatternName = name,
  Ast.asPatternInner = inner}

asPatternName :: (Ast.AsPattern -> Ast.Name)
asPatternName = Ast.asPatternName

asPatternInner :: (Ast.AsPattern -> Ast.Pattern)
asPatternInner = Ast.asPatternInner

asPatternWithName :: (Ast.AsPattern -> Ast.Name -> Ast.AsPattern)
asPatternWithName original newVal = Ast.AsPattern {
  Ast.asPatternName = newVal,
  Ast.asPatternInner = (Ast.asPatternInner original)}

asPatternWithInner :: (Ast.AsPattern -> Ast.Pattern -> Ast.AsPattern)
asPatternWithInner original newVal = Ast.AsPattern {
  Ast.asPatternName = (Ast.asPatternName original),
  Ast.asPatternInner = newVal}

recordPattern :: (Ast.Name -> [Ast.PatternField] -> Ast.RecordPattern)
recordPattern name fields = Ast.RecordPattern {
  Ast.recordPatternName = name,
  Ast.recordPatternFields = fields}

recordPatternName :: (Ast.RecordPattern -> Ast.Name)
recordPatternName = Ast.recordPatternName

recordPatternFields :: (Ast.RecordPattern -> [Ast.PatternField])
recordPatternFields = Ast.recordPatternFields

recordPatternWithName :: (Ast.RecordPattern -> Ast.Name -> Ast.RecordPattern)
recordPatternWithName original newVal = Ast.RecordPattern {
  Ast.recordPatternName = newVal,
  Ast.recordPatternFields = (Ast.recordPatternFields original)}

recordPatternWithFields :: (Ast.RecordPattern -> [Ast.PatternField] -> Ast.RecordPattern)
recordPatternWithFields original newVal = Ast.RecordPattern {
  Ast.recordPatternName = (Ast.recordPatternName original),
  Ast.recordPatternFields = newVal}

typedPattern :: (Ast.Pattern -> Ast.Type -> Ast.TypedPattern)
typedPattern inner type_ = Ast.TypedPattern {
  Ast.typedPatternInner = inner,
  Ast.typedPatternType = type_}

typedPatternInner :: (Ast.TypedPattern -> Ast.Pattern)
typedPatternInner = Ast.typedPatternInner

typedPatternType :: (Ast.TypedPattern -> Ast.Type)
typedPatternType = Ast.typedPatternType

typedPatternWithInner :: (Ast.TypedPattern -> Ast.Pattern -> Ast.TypedPattern)
typedPatternWithInner original newVal = Ast.TypedPattern {
  Ast.typedPatternInner = newVal,
  Ast.typedPatternType = (Ast.typedPatternType original)}

typedPatternWithType :: (Ast.TypedPattern -> Ast.Type -> Ast.TypedPattern)
typedPatternWithType original newVal = Ast.TypedPattern {
  Ast.typedPatternInner = (Ast.typedPatternInner original),
  Ast.typedPatternType = newVal}

patternField :: (Ast.Name -> Ast.Pattern -> Ast.PatternField)
patternField name pattern = Ast.PatternField {
  Ast.patternFieldName = name,
  Ast.patternFieldPattern = pattern}

patternFieldName :: (Ast.PatternField -> Ast.Name)
patternFieldName = Ast.patternFieldName

patternFieldPattern :: (Ast.PatternField -> Ast.Pattern)
patternFieldPattern = Ast.patternFieldPattern

patternFieldWithName :: (Ast.PatternField -> Ast.Name -> Ast.PatternField)
patternFieldWithName original newVal = Ast.PatternField {
  Ast.patternFieldName = newVal,
  Ast.patternFieldPattern = (Ast.patternFieldPattern original)}

patternFieldWithPattern :: (Ast.PatternField -> Ast.Pattern -> Ast.PatternField)
patternFieldWithPattern original newVal = Ast.PatternField {
  Ast.patternFieldName = (Ast.patternFieldName original),
  Ast.patternFieldPattern = newVal}

qualifiedName :: ([Ast.NamePart] -> Ast.NamePart -> Ast.QualifiedName)
qualifiedName qualifiers unqualified = Ast.QualifiedName {
  Ast.qualifiedNameQualifiers = qualifiers,
  Ast.qualifiedNameUnqualified = unqualified}

qualifiedNameQualifiers :: (Ast.QualifiedName -> [Ast.NamePart])
qualifiedNameQualifiers = Ast.qualifiedNameQualifiers

qualifiedNameUnqualified :: (Ast.QualifiedName -> Ast.NamePart)
qualifiedNameUnqualified = Ast.qualifiedNameUnqualified

qualifiedNameWithQualifiers :: (Ast.QualifiedName -> [Ast.NamePart] -> Ast.QualifiedName)
qualifiedNameWithQualifiers original newVal = Ast.QualifiedName {
  Ast.qualifiedNameQualifiers = newVal,
  Ast.qualifiedNameUnqualified = (Ast.qualifiedNameUnqualified original)}

qualifiedNameWithUnqualified :: (Ast.QualifiedName -> Ast.NamePart -> Ast.QualifiedName)
qualifiedNameWithUnqualified original newVal = Ast.QualifiedName {
  Ast.qualifiedNameQualifiers = (Ast.qualifiedNameQualifiers original),
  Ast.qualifiedNameUnqualified = newVal}

rightHandSide :: (Ast.Expression -> Ast.RightHandSide)
rightHandSide x = (Ast.RightHandSide x)

unRightHandSide :: (Ast.RightHandSide -> Ast.Expression)
unRightHandSide = Ast.unRightHandSide

statement :: (Ast.Expression -> Ast.Statement)
statement x = (Ast.Statement x)

unStatement :: (Ast.Statement -> Ast.Expression)
unStatement = Ast.unStatement

typeApplication :: (Ast.ApplicationType -> Ast.Type)
typeApplication x = (Ast.TypeApplication x)

typeCtx :: (Ast.ContextType -> Ast.Type)
typeCtx x = (Ast.TypeCtx x)

typeFunction :: (Ast.FunctionType -> Ast.Type)
typeFunction x = (Ast.TypeFunction x)

typeInfix :: (Ast.InfixType -> Ast.Type)
typeInfix x = (Ast.TypeInfix x)

typeList :: (Ast.Type -> Ast.Type)
typeList x = (Ast.TypeList x)

typeParens :: (Ast.Type -> Ast.Type)
typeParens x = (Ast.TypeParens x)

typeTuple :: ([Ast.Type] -> Ast.Type)
typeTuple x = (Ast.TypeTuple x)

typeVariable :: (Ast.Name -> Ast.Type)
typeVariable x = (Ast.TypeVariable x)

applicationType :: (Ast.Type -> Ast.Type -> Ast.ApplicationType)
applicationType context argument = Ast.ApplicationType {
  Ast.applicationTypeContext = context,
  Ast.applicationTypeArgument = argument}

applicationTypeContext :: (Ast.ApplicationType -> Ast.Type)
applicationTypeContext = Ast.applicationTypeContext

applicationTypeArgument :: (Ast.ApplicationType -> Ast.Type)
applicationTypeArgument = Ast.applicationTypeArgument

applicationTypeWithContext :: (Ast.ApplicationType -> Ast.Type -> Ast.ApplicationType)
applicationTypeWithContext original newVal = Ast.ApplicationType {
  Ast.applicationTypeContext = newVal,
  Ast.applicationTypeArgument = (Ast.applicationTypeArgument original)}

applicationTypeWithArgument :: (Ast.ApplicationType -> Ast.Type -> Ast.ApplicationType)
applicationTypeWithArgument original newVal = Ast.ApplicationType {
  Ast.applicationTypeContext = (Ast.applicationTypeContext original),
  Ast.applicationTypeArgument = newVal}

contextType :: (Ast.Assertion -> Ast.Type -> Ast.ContextType)
contextType ctx type_ = Ast.ContextType {
  Ast.contextTypeCtx = ctx,
  Ast.contextTypeType = type_}

contextTypeCtx :: (Ast.ContextType -> Ast.Assertion)
contextTypeCtx = Ast.contextTypeCtx

contextTypeType :: (Ast.ContextType -> Ast.Type)
contextTypeType = Ast.contextTypeType

contextTypeWithCtx :: (Ast.ContextType -> Ast.Assertion -> Ast.ContextType)
contextTypeWithCtx original newVal = Ast.ContextType {
  Ast.contextTypeCtx = newVal,
  Ast.contextTypeType = (Ast.contextTypeType original)}

contextTypeWithType :: (Ast.ContextType -> Ast.Type -> Ast.ContextType)
contextTypeWithType original newVal = Ast.ContextType {
  Ast.contextTypeCtx = (Ast.contextTypeCtx original),
  Ast.contextTypeType = newVal}

functionType :: (Ast.Type -> Ast.Type -> Ast.FunctionType)
functionType domain codomain = Ast.FunctionType {
  Ast.functionTypeDomain = domain,
  Ast.functionTypeCodomain = codomain}

functionTypeDomain :: (Ast.FunctionType -> Ast.Type)
functionTypeDomain = Ast.functionTypeDomain

functionTypeCodomain :: (Ast.FunctionType -> Ast.Type)
functionTypeCodomain = Ast.functionTypeCodomain

functionTypeWithDomain :: (Ast.FunctionType -> Ast.Type -> Ast.FunctionType)
functionTypeWithDomain original newVal = Ast.FunctionType {
  Ast.functionTypeDomain = newVal,
  Ast.functionTypeCodomain = (Ast.functionTypeCodomain original)}

functionTypeWithCodomain :: (Ast.FunctionType -> Ast.Type -> Ast.FunctionType)
functionTypeWithCodomain original newVal = Ast.FunctionType {
  Ast.functionTypeDomain = (Ast.functionTypeDomain original),
  Ast.functionTypeCodomain = newVal}

infixType :: (Ast.Type -> Ast.Operator -> Ast.Operator -> Ast.InfixType)
infixType lhs operator rhs = Ast.InfixType {
  Ast.infixTypeLhs = lhs,
  Ast.infixTypeOperator = operator,
  Ast.infixTypeRhs = rhs}

infixTypeLhs :: (Ast.InfixType -> Ast.Type)
infixTypeLhs = Ast.infixTypeLhs

infixTypeOperator :: (Ast.InfixType -> Ast.Operator)
infixTypeOperator = Ast.infixTypeOperator

infixTypeRhs :: (Ast.InfixType -> Ast.Operator)
infixTypeRhs = Ast.infixTypeRhs

infixTypeWithLhs :: (Ast.InfixType -> Ast.Type -> Ast.InfixType)
infixTypeWithLhs original newVal = Ast.InfixType {
  Ast.infixTypeLhs = newVal,
  Ast.infixTypeOperator = (Ast.infixTypeOperator original),
  Ast.infixTypeRhs = (Ast.infixTypeRhs original)}

infixTypeWithOperator :: (Ast.InfixType -> Ast.Operator -> Ast.InfixType)
infixTypeWithOperator original newVal = Ast.InfixType {
  Ast.infixTypeLhs = (Ast.infixTypeLhs original),
  Ast.infixTypeOperator = newVal,
  Ast.infixTypeRhs = (Ast.infixTypeRhs original)}

infixTypeWithRhs :: (Ast.InfixType -> Ast.Operator -> Ast.InfixType)
infixTypeWithRhs original newVal = Ast.InfixType {
  Ast.infixTypeLhs = (Ast.infixTypeLhs original),
  Ast.infixTypeOperator = (Ast.infixTypeOperator original),
  Ast.infixTypeRhs = newVal}

typeDeclaration :: (Ast.DeclarationHead -> Ast.Type -> Ast.TypeDeclaration)
typeDeclaration name type_ = Ast.TypeDeclaration {
  Ast.typeDeclarationName = name,
  Ast.typeDeclarationType = type_}

typeDeclarationName :: (Ast.TypeDeclaration -> Ast.DeclarationHead)
typeDeclarationName = Ast.typeDeclarationName

typeDeclarationType :: (Ast.TypeDeclaration -> Ast.Type)
typeDeclarationType = Ast.typeDeclarationType

typeDeclarationWithName :: (Ast.TypeDeclaration -> Ast.DeclarationHead -> Ast.TypeDeclaration)
typeDeclarationWithName original newVal = Ast.TypeDeclaration {
  Ast.typeDeclarationName = newVal,
  Ast.typeDeclarationType = (Ast.typeDeclarationType original)}

typeDeclarationWithType :: (Ast.TypeDeclaration -> Ast.Type -> Ast.TypeDeclaration)
typeDeclarationWithType original newVal = Ast.TypeDeclaration {
  Ast.typeDeclarationName = (Ast.typeDeclarationName original),
  Ast.typeDeclarationType = newVal}

typeSignature :: (Ast.Name -> Ast.Type -> Ast.TypeSignature)
typeSignature name type_ = Ast.TypeSignature {
  Ast.typeSignatureName = name,
  Ast.typeSignatureType = type_}

typeSignatureName :: (Ast.TypeSignature -> Ast.Name)
typeSignatureName = Ast.typeSignatureName

typeSignatureType :: (Ast.TypeSignature -> Ast.Type)
typeSignatureType = Ast.typeSignatureType

typeSignatureWithName :: (Ast.TypeSignature -> Ast.Name -> Ast.TypeSignature)
typeSignatureWithName original newVal = Ast.TypeSignature {
  Ast.typeSignatureName = newVal,
  Ast.typeSignatureType = (Ast.typeSignatureType original)}

typeSignatureWithType :: (Ast.TypeSignature -> Ast.Type -> Ast.TypeSignature)
typeSignatureWithType original newVal = Ast.TypeSignature {
  Ast.typeSignatureName = (Ast.typeSignatureName original),
  Ast.typeSignatureType = newVal}

typedBinding :: (Ast.TypeSignature -> Ast.ValueBinding -> Ast.TypedBinding)
typedBinding typeSignature valueBinding = Ast.TypedBinding {
  Ast.typedBindingTypeSignature = typeSignature,
  Ast.typedBindingValueBinding = valueBinding}

typedBindingTypeSignature :: (Ast.TypedBinding -> Ast.TypeSignature)
typedBindingTypeSignature = Ast.typedBindingTypeSignature

typedBindingValueBinding :: (Ast.TypedBinding -> Ast.ValueBinding)
typedBindingValueBinding = Ast.typedBindingValueBinding

typedBindingWithTypeSignature :: (Ast.TypedBinding -> Ast.TypeSignature -> Ast.TypedBinding)
typedBindingWithTypeSignature original newVal = Ast.TypedBinding {
  Ast.typedBindingTypeSignature = newVal,
  Ast.typedBindingValueBinding = (Ast.typedBindingValueBinding original)}

typedBindingWithValueBinding :: (Ast.TypedBinding -> Ast.ValueBinding -> Ast.TypedBinding)
typedBindingWithValueBinding original newVal = Ast.TypedBinding {
  Ast.typedBindingTypeSignature = (Ast.typedBindingTypeSignature original),
  Ast.typedBindingValueBinding = newVal}

valueBindingSimple :: (Ast.SimpleValueBinding -> Ast.ValueBinding)
valueBindingSimple x = (Ast.ValueBindingSimple x)

simpleValueBinding :: (Ast.Pattern -> Ast.RightHandSide -> Maybe Ast.LocalBindings -> Ast.SimpleValueBinding)
simpleValueBinding pattern rhs localBindings = Ast.SimpleValueBinding {
  Ast.simpleValueBindingPattern = pattern,
  Ast.simpleValueBindingRhs = rhs,
  Ast.simpleValueBindingLocalBindings = localBindings}

simpleValueBindingPattern :: (Ast.SimpleValueBinding -> Ast.Pattern)
simpleValueBindingPattern = Ast.simpleValueBindingPattern

simpleValueBindingRhs :: (Ast.SimpleValueBinding -> Ast.RightHandSide)
simpleValueBindingRhs = Ast.simpleValueBindingRhs

simpleValueBindingLocalBindings :: (Ast.SimpleValueBinding -> Maybe Ast.LocalBindings)
simpleValueBindingLocalBindings = Ast.simpleValueBindingLocalBindings

simpleValueBindingWithPattern :: (Ast.SimpleValueBinding -> Ast.Pattern -> Ast.SimpleValueBinding)
simpleValueBindingWithPattern original newVal = Ast.SimpleValueBinding {
  Ast.simpleValueBindingPattern = newVal,
  Ast.simpleValueBindingRhs = (Ast.simpleValueBindingRhs original),
  Ast.simpleValueBindingLocalBindings = (Ast.simpleValueBindingLocalBindings original)}

simpleValueBindingWithRhs :: (Ast.SimpleValueBinding -> Ast.RightHandSide -> Ast.SimpleValueBinding)
simpleValueBindingWithRhs original newVal = Ast.SimpleValueBinding {
  Ast.simpleValueBindingPattern = (Ast.simpleValueBindingPattern original),
  Ast.simpleValueBindingRhs = newVal,
  Ast.simpleValueBindingLocalBindings = (Ast.simpleValueBindingLocalBindings original)}

simpleValueBindingWithLocalBindings :: (Ast.SimpleValueBinding -> Maybe Ast.LocalBindings -> Ast.SimpleValueBinding)
simpleValueBindingWithLocalBindings original newVal = Ast.SimpleValueBinding {
  Ast.simpleValueBindingPattern = (Ast.simpleValueBindingPattern original),
  Ast.simpleValueBindingRhs = (Ast.simpleValueBindingRhs original),
  Ast.simpleValueBindingLocalBindings = newVal}

variable :: (Ast.Name -> Ast.Variable)
variable x = (Ast.Variable x)

unVariable :: (Ast.Variable -> Ast.Name)
unVariable = Ast.unVariable
