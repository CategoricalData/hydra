/**
 * A union of scalars supported in the YAML failsafe and JSON schemas. Other scalars are not supported here
 */
package hydra.ext.yaml.representation

/**
 * A union of scalars supported in the YAML failsafe and JSON schemas. Other scalars are not supported here
 */
enum Scalar:
    /**
     * Represents a true/false value
     * 
     * @comments JSON schema: tag:yaml.org,2002:bool
     * @type boolean
     */
    case bool(value: Boolean) extends Scalar
    /**
     * Represents an approximation to real numbers
     * 
     * @comments JSON schema: tag:yaml.org,2002:float
     *           In addition to arbitrary-precision floating-point numbers in scientific notation, YAML allows for three
     * special values, which are not supported here: positive and negative infinity (.inf and -.inf), and "not a number
     * (.nan)
     * @type float:
     *         precision: arbitrary
     */
    case float(value: Double) extends Scalar
    /**
     * Represents arbitrary sized finite mathematical integers
     * 
     * @comments JSON schema: tag:yaml.org,2002:int
     * @type integer:
     *         precision: arbitrary
     */
    case int(value: Long) extends Scalar
    /**
     * Represents the lack of a value
     * 
     * @comments JSON schema: tag:yaml.org,2002:null
     */
    case `null`() extends Scalar
    /**
     * @comments Failsafe schema: tag:yaml.org,2002:str
     * @type string
     */
    case str(value: String) extends Scalar

val _Scalar: String = "hydra/ext/yaml/representation.Scalar"
val _Scalar_bool: String = "bool"
val _Scalar_float: String = "float"
val _Scalar_int: String = "int"
val _Scalar_null: String = "null"
val _Scalar_str: String = "str"
