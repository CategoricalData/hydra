package hydra.core

enum Precision:
    case arbitrary() extends Precision
    /**
     * @type integer
     */
    case bits(value: Int) extends Precision

val _Precision: String = "hydra/core.Precision"
val _Precision_arbitrary: String = "arbitrary"
val _Precision_bits: String = "bits"
