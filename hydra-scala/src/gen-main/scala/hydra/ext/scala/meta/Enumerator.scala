package hydra.ext.scala.meta

enum Enumerator:
    /**
     * @type hydra/ext/scala/meta.Enumerator.Generator
     */
    case generator(value: hydra.ext.scala.meta.Enumerator.Generator) extends Enumerator
    /**
     * @type hydra/ext/scala/meta.Enumerator.CaseGenerator
     */
    case caseGenerator(value: hydra.ext.scala.meta.Enumerator.CaseGenerator) extends Enumerator
    /**
     * @type hydra/ext/scala/meta.Enumerator.Val
     */
    case `val`(value: hydra.ext.scala.meta.Enumerator.Val) extends Enumerator
    /**
     * @type hydra/ext/scala/meta.Enumerator.Guard
     */
    case guard(value: hydra.ext.scala.meta.Enumerator.Guard) extends Enumerator
object Enumerator {
    case class Generator (
        /**
         * @type hydra/ext/scala/meta.Pat
         * 
         * @type hydra/ext/scala/meta.Pat
         */
        pat: hydra.ext.scala.meta.Pat,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        rhs: hydra.ext.scala.meta.Term
    )
    
    case class CaseGenerator (
        /**
         * @type hydra/ext/scala/meta.Pat
         * 
         * @type hydra/ext/scala/meta.Pat
         */
        pat: hydra.ext.scala.meta.Pat,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        rhs: hydra.ext.scala.meta.Term
    )
    
    case class Val (
        /**
         * @type hydra/ext/scala/meta.Pat
         * 
         * @type hydra/ext/scala/meta.Pat
         */
        pat: hydra.ext.scala.meta.Pat,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        rhs: hydra.ext.scala.meta.Term
    )
    
    case class Guard (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        cond: hydra.ext.scala.meta.Term
    )
}

val _Enumerator: String = "hydra/ext/scala/meta.Enumerator"
val _Enumerator_CaseGenerator: String = "hydra/ext/scala/meta.Enumerator.CaseGenerator"
val _Enumerator_CaseGenerator_pat: String = "pat"
val _Enumerator_CaseGenerator_rhs: String = "rhs"
val _Enumerator_Generator: String = "hydra/ext/scala/meta.Enumerator.Generator"
val _Enumerator_Generator_pat: String = "pat"
val _Enumerator_Generator_rhs: String = "rhs"
val _Enumerator_Guard: String = "hydra/ext/scala/meta.Enumerator.Guard"
val _Enumerator_Guard_cond: String = "cond"
val _Enumerator_Val: String = "hydra/ext/scala/meta.Enumerator.Val"
val _Enumerator_Val_pat: String = "pat"
val _Enumerator_Val_rhs: String = "rhs"
val _Enumerator_caseGenerator: String = "caseGenerator"
val _Enumerator_generator: String = "generator"
val _Enumerator_guard: String = "guard"
val _Enumerator_val: String = "val"
