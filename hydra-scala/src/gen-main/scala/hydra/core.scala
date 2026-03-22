package hydra.core

case class AnnotatedTerm(body: hydra.core.Term, annotation: Map[hydra.core.Name, hydra.core.Term])

case class AnnotatedType(body: hydra.core.Type, annotation: Map[hydra.core.Name, hydra.core.Term])

case class Application(function: hydra.core.Term, argument: hydra.core.Term)

case class ApplicationType(function: hydra.core.Type, argument: hydra.core.Type)

case class Binding(name: hydra.core.Name, term: hydra.core.Term, `type`: Option[hydra.core.TypeScheme])

case class CaseStatement(typeName: hydra.core.Name, default: Option[hydra.core.Term], cases: Seq[hydra.core.Field])

case class EitherType(left: hydra.core.Type, right: hydra.core.Type)

case class PairType(first: hydra.core.Type, second: hydra.core.Type)

enum Elimination :
   case record(value: hydra.core.Projection) extends Elimination
   case union(value: hydra.core.CaseStatement) extends Elimination
   case wrap(value: hydra.core.Name) extends Elimination

case class Field(name: hydra.core.Name, term: hydra.core.Term)

case class FieldType(name: hydra.core.Name, `type`: hydra.core.Type)

enum FloatType :
   case bigfloat extends FloatType
   case float32 extends FloatType
   case float64 extends FloatType

enum FloatValue :
   case bigfloat(value: BigDecimal) extends FloatValue
   case float32(value: Float) extends FloatValue
   case float64(value: Double) extends FloatValue

case class ForallType(parameter: hydra.core.Name, body: hydra.core.Type)

enum Function :
   case elimination(value: hydra.core.Elimination) extends Function
   case lambda(value: hydra.core.Lambda) extends Function
   case primitive(value: hydra.core.Name) extends Function

case class FunctionType(domain: hydra.core.Type, codomain: hydra.core.Type)

case class Injection(typeName: hydra.core.Name, field: hydra.core.Field)

enum IntegerType :
   case bigint extends IntegerType
   case int8 extends IntegerType
   case int16 extends IntegerType
   case int32 extends IntegerType
   case int64 extends IntegerType
   case uint8 extends IntegerType
   case uint16 extends IntegerType
   case uint32 extends IntegerType
   case uint64 extends IntegerType

enum IntegerValue :
   case bigint(value: BigInt) extends IntegerValue
   case int8(value: Byte) extends IntegerValue
   case int16(value: Short) extends IntegerValue
   case int32(value: Int) extends IntegerValue
   case int64(value: Long) extends IntegerValue
   case uint8(value: Byte) extends IntegerValue
   case uint16(value: Int) extends IntegerValue
   case uint32(value: Long) extends IntegerValue
   case uint64(value: BigInt) extends IntegerValue

case class Lambda(parameter: hydra.core.Name, domain: Option[hydra.core.Type], body: hydra.core.Term)

case class Let(bindings: Seq[hydra.core.Binding], body: hydra.core.Term)

enum Literal :
   case binary(value: scala.Predef.String) extends Literal
   case boolean(value: Boolean) extends Literal
   case float(value: hydra.core.FloatValue) extends Literal
   case integer(value: hydra.core.IntegerValue) extends Literal
   case string(value: scala.Predef.String) extends Literal

enum LiteralType :
   case binary extends LiteralType
   case boolean extends LiteralType
   case float(value: hydra.core.FloatType) extends LiteralType
   case integer(value: hydra.core.IntegerType) extends LiteralType
   case string extends LiteralType

case class MapType(keys: hydra.core.Type, values: hydra.core.Type)

type Name = scala.Predef.String

case class Projection(typeName: hydra.core.Name, field: hydra.core.Name)

case class Record(typeName: hydra.core.Name, fields: Seq[hydra.core.Field])

enum Term :
   case annotated(value: hydra.core.AnnotatedTerm) extends Term
   case application(value: hydra.core.Application) extends Term
   case either(value: Either[hydra.core.Term, hydra.core.Term]) extends Term
   case function(value: hydra.core.Function) extends Term
   case let(value: hydra.core.Let) extends Term
   case list(value: Seq[hydra.core.Term]) extends Term
   case literal(value: hydra.core.Literal) extends Term
   case map(value: Map[hydra.core.Term, hydra.core.Term]) extends Term
   case maybe(value: Option[hydra.core.Term]) extends Term
   case pair(value: Tuple2[hydra.core.Term, hydra.core.Term]) extends Term
   case record(value: hydra.core.Record) extends Term
   case set(value: scala.collection.immutable.Set[hydra.core.Term]) extends Term
   case typeApplication(value: hydra.core.TypeApplicationTerm) extends Term
   case typeLambda(value: hydra.core.TypeLambda) extends Term
   case union(value: hydra.core.Injection) extends Term
   case unit extends Term
   case variable(value: hydra.core.Name) extends Term
   case wrap(value: hydra.core.WrappedTerm) extends Term

enum Type :
   case annotated(value: hydra.core.AnnotatedType) extends Type
   case application(value: hydra.core.ApplicationType) extends Type
   case either(value: hydra.core.EitherType) extends Type
   case forall(value: hydra.core.ForallType) extends Type
   case function(value: hydra.core.FunctionType) extends Type
   case list(value: hydra.core.Type) extends Type
   case literal(value: hydra.core.LiteralType) extends Type
   case map(value: hydra.core.MapType) extends Type
   case maybe(value: hydra.core.Type) extends Type
   case pair(value: hydra.core.PairType) extends Type
   case record(value: Seq[hydra.core.FieldType]) extends Type
   case set(value: hydra.core.Type) extends Type
   case union(value: Seq[hydra.core.FieldType]) extends Type
   case unit extends Type
   case variable(value: hydra.core.Name) extends Type
   case void extends Type
   case wrap(value: hydra.core.Type) extends Type

case class TypeApplicationTerm(body: hydra.core.Term, `type`: hydra.core.Type)

case class TypeLambda(parameter: hydra.core.Name, body: hydra.core.Term)

case class TypeScheme(variables: Seq[hydra.core.Name], `type`: hydra.core.Type, constraints: Option[Map[hydra.core.Name, hydra.core.TypeVariableMetadata]])

case class TypeVariableMetadata(classes: scala.collection.immutable.Set[hydra.core.Name])

case class WrappedTerm(typeName: hydra.core.Name, body: hydra.core.Term)
