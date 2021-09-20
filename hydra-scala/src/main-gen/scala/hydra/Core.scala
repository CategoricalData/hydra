/**
 * The Hydra Core model (in progress)
 */

package org.example.hydra



object Core{
    /**
     * A term which applies a function to an argument
     */
    case class Application(
        /**
         * @type hydra/core.Term
         */
        function: Term,
        
        /**
         * @type hydra/core.Term
         */
        argument: Term
    )
    
    /**
     * Any of a fixed set of atomic types, also called base types, primitive types, or type constants
     * 
     * @comments The so-called term constants, or valid values, of each atomic type are unspecified
     */
    sealed trait AtomicType
    final case class AtomicType_binary() extends AtomicType
    final case class AtomicType_boolean() extends AtomicType
    /**
     * @type hydra/core.FloatType
     */
    final case class AtomicType_float(value: FloatType) extends AtomicType
    /**
     * @type hydra/core.IntegerType
     */
    final case class AtomicType_integer(value: IntegerType) extends AtomicType
    final case class AtomicType_string() extends AtomicType
    
    /**
     * A term constant; an instance of an atomic type
     */
    sealed trait AtomicValue
    /**
     * @type binary
     */
    final case class AtomicValue_binary(value: String) extends AtomicValue
    /**
     * @type hydra/core.BooleanValue
     */
    final case class AtomicValue_boolean(value: BooleanValue) extends AtomicValue
    /**
     * @type hydra/core.FloatValue
     */
    final case class AtomicValue_float(value: FloatValue) extends AtomicValue
    /**
     * @type hydra/core.IntegerValue
     */
    final case class AtomicValue_integer(value: IntegerValue) extends AtomicValue
    /**
     * @type string
     */
    final case class AtomicValue_string(value: String) extends AtomicValue
    
    sealed trait BooleanValue
    case object BooleanValue_false extends BooleanValue
    case object BooleanValue_true extends BooleanValue
    
    case class CaseStatement(
        /**
         * A handler for each alternative in a union type. The term of each case must be function-typed.
         * 
         * @type list: hydra/core.Field
         */
        cases: Seq[Field],
        
        /**
         * A convenience which allows certain "don't care" cases to be omitted. The result is a term which does not otherwise
         * depend on the variant value.
         * 
         * @type hydra/core.Term
         */
        default: Term
    )
    
    /**
     * A labeled term
     */
    case class Field(
        /**
         * @type hydra/core.FieldName
         */
        name: FieldName,
        
        /**
         * @type hydra/core.Term
         */
        term: Term
    )
    
    /**
     * @type string
     */
    type FieldName = String
    
    case class FieldType(
        /**
         * @type hydra/core.FieldName
         */
        name: FieldName,
        
        /**
         * @type hydra/core.Type
         */
        `type`: Type
    )
    
    sealed trait FloatType
    case object FloatType_bigfloat extends FloatType
    case object FloatType_float32 extends FloatType
    case object FloatType_float64 extends FloatType
    
    sealed trait FloatValue
    /**
     * @type float:
     *         precision: arbitrary
     */
    final case class FloatValue_bigfloat(value: Double) extends FloatValue
    /**
     * @type float
     */
    final case class FloatValue_float32(value: Float) extends FloatValue
    /**
     * @type float:
     *         precision:
     *           bits: 64
     */
    final case class FloatValue_float64(value: Double) extends FloatValue
    
    /**
     * A function type, also known as an arrow type
     */
    case class FunctionType(
        /**
         * @type hydra/core.Type
         */
        domain: Type,
        
        /**
         * @type hydra/core.Type
         */
        codomain: Type
    )
    
    sealed trait IntegerType
    case object IntegerType_bigint extends IntegerType
    case object IntegerType_int8 extends IntegerType
    case object IntegerType_int16 extends IntegerType
    case object IntegerType_int32 extends IntegerType
    case object IntegerType_int64 extends IntegerType
    case object IntegerType_uint8 extends IntegerType
    case object IntegerType_uint16 extends IntegerType
    case object IntegerType_uint32 extends IntegerType
    case object IntegerType_uint64 extends IntegerType
    
    sealed trait IntegerValue
    /**
     * @type integer:
     *         precision: arbitrary
     */
    final case class IntegerValue_bigint(value: Long) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 8
     */
    final case class IntegerValue_int8(value: Byte) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 16
     */
    final case class IntegerValue_int16(value: Short) extends IntegerValue
    /**
     * @type integer
     */
    final case class IntegerValue_int32(value: Int) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 64
     */
    final case class IntegerValue_int64(value: Long) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 8
     *         signed: false
     */
    final case class IntegerValue_uint8(value: Byte) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 16
     *         signed: false
     */
    final case class IntegerValue_uint16(value: Short) extends IntegerValue
    /**
     * @type integer:
     *         signed: false
     */
    final case class IntegerValue_uint32(value: Int) extends IntegerValue
    /**
     * @type integer:
     *         precision:
     *           bits: 64
     *         signed: false
     */
    final case class IntegerValue_uint64(value: Long) extends IntegerValue
    
    /**
     * A function abstraction (lambda)
     */
    case class Lambda(
        /**
         * The parameter of the lambda
         * 
         * @type hydra/core.Variable
         */
        parameter: Variable,
        
        /**
         * The body of the lambda
         * 
         * @type hydra/core.Term
         */
        body: Term
    )
    
    /**
     * @type string
     */
    type Name = String
    
    sealed trait Term
    /**
     * A function application
     * 
     * @type hydra/core.Application
     */
    final case class Term_application(value: Application) extends Term
    /**
     * An atomic value
     * 
     * @type hydra/core.AtomicValue
     */
    final case class Term_atomic(value: AtomicValue) extends Term
    /**
     * A case statement applied to a variant record
     * 
     * @type hydra/core.CaseStatement
     */
    final case class Term_cases(value: CaseStatement) extends Term
    /**
     * Hydra's delta function, which maps an element to its data term
     */
    final case class Term_data() extends Term
    /**
     * An element reference
     * 
     * @type hydra/core.Name
     */
    final case class Term_element(value: Name) extends Term
    /**
     * A reference to a built-in function
     * 
     * @type hydra/core.Name
     */
    final case class Term_function(value: Name) extends Term
    /**
     * A function abstraction (lambda)
     * 
     * @type hydra/core.Lambda
     */
    final case class Term_lambda(value: Lambda) extends Term
    /**
     * A list
     * 
     * @type list: hydra/core.Term
     */
    final case class Term_list(value: Seq[Term]) extends Term
    /**
     * A projection of a field from a record
     * 
     * @type hydra/core.FieldName
     */
    final case class Term_projection(value: FieldName) extends Term
    /**
     * A record, or labeled tuple
     * 
     * @type list: hydra/core.Field
     */
    final case class Term_record(value: Seq[Field]) extends Term
    /**
     * A union term, i.e. a generalization of inl() or inr()
     * 
     * @type hydra/core.Field
     */
    final case class Term_union(value: Field) extends Term
    /**
     * A variable reference
     * 
     * @type hydra/core.Variable
     */
    final case class Term_variable(value: Variable) extends Term
    
    sealed trait Type
    /**
     * @type hydra/core.AtomicType
     */
    final case class Type_atomic(value: AtomicType) extends Type
    /**
     * @type hydra/core.Type
     */
    final case class Type_element(value: Type) extends Type
    /**
     * @type hydra/core.FunctionType
     */
    final case class Type_function(value: FunctionType) extends Type
    /**
     * @type hydra/core.Type
     */
    final case class Type_list(value: Type) extends Type
    /**
     * @type hydra/core.Name
     */
    final case class Type_nominal(value: Name) extends Type
    /**
     * @type list: hydra/core.FieldType
     */
    final case class Type_record(value: Seq[FieldType]) extends Type
    /**
     * @type list: hydra/core.FieldType
     */
    final case class Type_union(value: Seq[FieldType]) extends Type
    
    /**
     * A symbol which stands in for a term
     * 
     * @type string
     */
    type Variable = String
}
