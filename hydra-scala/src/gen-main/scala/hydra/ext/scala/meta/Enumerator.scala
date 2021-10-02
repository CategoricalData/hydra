package hydra.ext.scala.meta

enum Enumerator:
    /**
     * @type hydra/ext/scala/meta.Enumerator.Generator
     */
    case generator(value: Enumerator_Generator) extends Enumerator
    /**
     * @type hydra/ext/scala/meta.Enumerator.CaseGenerator
     */
    case caseGenerator(value: Enumerator_CaseGenerator) extends Enumerator
    /**
     * @type hydra/ext/scala/meta.Enumerator.Val
     */
    case `val`(value: Enumerator_Val) extends Enumerator
    /**
     * @type hydra/ext/scala/meta.Enumerator.Guard
     */
    case guard(value: Enumerator_Guard) extends Enumerator

val _Enumerator: String = "hydra/ext/scala/meta.Enumerator"
val _Enumerator_caseGenerator: String = "caseGenerator"
val _Enumerator_generator: String = "generator"
val _Enumerator_guard: String = "guard"
val _Enumerator_val: String = "val"
