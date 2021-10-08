package hydra.adapter

import hydra.evaluation.Step

case class Adapter[t, v](
    /**
     * @type variable: t
     */
    source: t,
    
    /**
     * @type variable: t
     */
    target: t,
    
    /**
     * @type parameterized:
     *         genericType: hydra/evaluation.Step
     *         parameters:
     *         - type:
     *             variable: v
     *           variable: a
     *         - type:
     *             variable: v
     *           variable: b
     */
    mapping: Step[v,v],
    
    /**
     * @type boolean
     */
    isLossy: Boolean,
    
    /**
     * @type list: string
     */
    notes: Seq[String]
)

val _Adapter: String = "hydra/adapter.Adapter"
val _Adapter_isLossy: String = "isLossy"
val _Adapter_mapping: String = "mapping"
val _Adapter_notes: String = "notes"
val _Adapter_source: String = "source"
val _Adapter_target: String = "target"
