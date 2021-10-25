package hydra.ext.scala.meta

enum Lit:
    case `null`() extends Lit
    /**
     * @type integer
     */
    case int(value: Int) extends Lit
    /**
     * @type float:
     *         precision:
     *           bits: 64
     */
    case double(value: Double) extends Lit
    /**
     * @type float
     */
    case float(value: Float) extends Lit
    /**
     * @type integer:
     *         precision:
     *           bits: 8
     */
    case byte(value: Byte) extends Lit
    /**
     * @type integer:
     *         precision:
     *           bits: 16
     */
    case short(value: Short) extends Lit
    /**
     * @type integer:
     *         precision:
     *           bits: 16
     *         signed: false
     */
    case char(value: Short) extends Lit
    /**
     * @type integer:
     *         precision:
     *           bits: 64
     */
    case long(value: Long) extends Lit
    /**
     * @type boolean
     */
    case boolean(value: Boolean) extends Lit
    case unit() extends Lit
    /**
     * @type string
     */
    case string(value: String) extends Lit
    /**
     * @type hydra/ext/scala/meta.ScalaSymbol
     */
    case symbol(value: hydra.ext.scala.meta.ScalaSymbol) extends Lit

val _Lit: String = "hydra/ext/scala/meta.Lit"
val _Lit_boolean: String = "boolean"
val _Lit_byte: String = "byte"
val _Lit_char: String = "char"
val _Lit_double: String = "double"
val _Lit_float: String = "float"
val _Lit_int: String = "int"
val _Lit_long: String = "long"
val _Lit_null: String = "null"
val _Lit_short: String = "short"
val _Lit_string: String = "string"
val _Lit_symbol: String = "symbol"
val _Lit_unit: String = "unit"
