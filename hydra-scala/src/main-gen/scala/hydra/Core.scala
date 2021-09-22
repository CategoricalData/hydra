/**
 * The Hydra Core model (in progress)
 */

package hydra



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
    enum AtomicType:
        case binary() extends AtomicType
        case boolean() extends AtomicType
        /**
         * @type hydra/core.FloatType
         */
        case float(value: FloatType) extends AtomicType
        /**
         * @type hydra/core.IntegerType
         */
        case integer(value: IntegerType) extends AtomicType
        case string() extends AtomicType
    
    /**
     * A term constant; an instance of an atomic type
     */
    enum AtomicValue:
        /**
         * @type binary
         */
        case binary(value: String) extends AtomicValue
        /**
         * @type hydra/core.BooleanValue
         */
        case boolean(value: BooleanValue) extends AtomicValue
        /**
         * @type hydra/core.FloatValue
         */
        case float(value: FloatValue) extends AtomicValue
        /**
         * @type hydra/core.IntegerValue
         */
        case integer(value: IntegerValue) extends AtomicValue
        /**
         * @type string
         */
        case string(value: String) extends AtomicValue
    
    enum AtomicVariant:
        case binary() extends AtomicVariant
        case boolean() extends AtomicVariant
        case float() extends AtomicVariant
        case integer() extends AtomicVariant
        case string() extends AtomicVariant
    
    enum BooleanValue:
        case `false`() extends BooleanValue
        case `true`() extends BooleanValue
    
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
     * An equality judgement: less than, equal to, or greater than
     */
    enum Comparison:
        case lessThan() extends Comparison
        case equalTo() extends Comparison
        case greaterThan() extends Comparison
    
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
    
    enum FloatType:
        case bigfloat() extends FloatType
        case float32() extends FloatType
        case float64() extends FloatType
    
    enum FloatValue:
        /**
         * @type float:
         *         precision: arbitrary
         */
        case bigfloat(value: Double) extends FloatValue
        /**
         * @type float
         */
        case float32(value: Float) extends FloatValue
        /**
         * @type float:
         *         precision:
         *           bits: 64
         */
        case float64(value: Double) extends FloatValue
    
    enum FloatVariant:
        case bigfloat() extends FloatVariant
        case float32() extends FloatVariant
        case float64() extends FloatVariant
    
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
    
    enum IntegerType:
        case bigint() extends IntegerType
        case int8() extends IntegerType
        case int16() extends IntegerType
        case int32() extends IntegerType
        case int64() extends IntegerType
        case uint8() extends IntegerType
        case uint16() extends IntegerType
        case uint32() extends IntegerType
        case uint64() extends IntegerType
    
    enum IntegerValue:
        /**
         * @type integer:
         *         precision: arbitrary
         */
        case bigint(value: Long) extends IntegerValue
        /**
         * @type integer:
         *         precision:
         *           bits: 8
         */
        case int8(value: Byte) extends IntegerValue
        /**
         * @type integer:
         *         precision:
         *           bits: 16
         */
        case int16(value: Short) extends IntegerValue
        /**
         * @type integer
         */
        case int32(value: Int) extends IntegerValue
        /**
         * @type integer:
         *         precision:
         *           bits: 64
         */
        case int64(value: Long) extends IntegerValue
        /**
         * @type integer:
         *         precision:
         *           bits: 8
         *         signed: false
         */
        case uint8(value: Byte) extends IntegerValue
        /**
         * @type integer:
         *         precision:
         *           bits: 16
         *         signed: false
         */
        case uint16(value: Short) extends IntegerValue
        /**
         * @type integer:
         *         signed: false
         */
        case uint32(value: Int) extends IntegerValue
        /**
         * @type integer:
         *         precision:
         *           bits: 64
         *         signed: false
         */
        case uint64(value: Long) extends IntegerValue
    
    enum IntegerVariant:
        case bigint() extends IntegerVariant
        case int8() extends IntegerVariant
        case int16() extends IntegerVariant
        case int32() extends IntegerVariant
        case int64() extends IntegerVariant
        case uint8() extends IntegerVariant
        case uint16() extends IntegerVariant
        case uint32() extends IntegerVariant
        case uint64() extends IntegerVariant
    
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
    
    enum Term:
        /**
         * A function application
         * 
         * @type hydra/core.Application
         */
        case application(value: Application) extends Term
        /**
         * An atomic value
         * 
         * @type hydra/core.AtomicValue
         */
        case atomic(value: AtomicValue) extends Term
        /**
         * A case statement applied to a variant record
         * 
         * @type hydra/core.CaseStatement
         */
        case cases(value: CaseStatement) extends Term
        /**
         * Compares a term with a given term of the same type, producing a Comparison
         * 
         * @type hydra/core.Term
         */
        case compareTo(value: Term) extends Term
        /**
         * Hydra's delta function, which maps an element to its data term
         */
        case data() extends Term
        /**
         * An element reference
         * 
         * @type hydra/core.Name
         */
        case element(value: Name) extends Term
        /**
         * A reference to a built-in function
         * 
         * @type hydra/core.Name
         */
        case function(value: Name) extends Term
        /**
         * A function abstraction (lambda)
         * 
         * @type hydra/core.Lambda
         */
        case lambda(value: Lambda) extends Term
        /**
         * A list
         * 
         * @type list: hydra/core.Term
         */
        case list(value: Seq[Term]) extends Term
        /**
         * A projection of a field from a record
         * 
         * @type hydra/core.FieldName
         */
        case projection(value: FieldName) extends Term
        /**
         * A record, or labeled tuple
         * 
         * @type list: hydra/core.Field
         */
        case record(value: Seq[Field]) extends Term
        /**
         * A union term, i.e. a generalization of inl() or inr()
         * 
         * @type hydra/core.Field
         */
        case union(value: Field) extends Term
        /**
         * A variable reference
         * 
         * @type hydra/core.Variable
         */
        case variable(value: Variable) extends Term
    
    enum TermVariant:
        case application() extends TermVariant
        case atomic() extends TermVariant
        case cases() extends TermVariant
        case compareTo() extends TermVariant
        case data() extends TermVariant
        case element() extends TermVariant
        case function() extends TermVariant
        case lambda() extends TermVariant
        case list() extends TermVariant
        case projection() extends TermVariant
        case record() extends TermVariant
        case union() extends TermVariant
        case variable() extends TermVariant
    
    enum Type:
        /**
         * @type hydra/core.AtomicType
         */
        case atomic(value: AtomicType) extends Type
        /**
         * @type hydra/core.Type
         */
        case element(value: Type) extends Type
        /**
         * @type hydra/core.FunctionType
         */
        case function(value: FunctionType) extends Type
        /**
         * @type hydra/core.Type
         */
        case list(value: Type) extends Type
        /**
         * @type hydra/core.Name
         */
        case nominal(value: Name) extends Type
        /**
         * @type list: hydra/core.FieldType
         */
        case record(value: Seq[FieldType]) extends Type
        /**
         * @type list: hydra/core.FieldType
         */
        case union(value: Seq[FieldType]) extends Type
    
    enum TypeVariant:
        case atomic() extends TypeVariant
        case element() extends TypeVariant
        case function() extends TypeVariant
        case list() extends TypeVariant
        case nominal() extends TypeVariant
        case record() extends TypeVariant
        case union() extends TypeVariant
    
    /**
     * A symbol which stands in for a term
     * 
     * @type string
     */
    type Variable = String
}
