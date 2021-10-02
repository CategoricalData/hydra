package hydra.ext.scala.meta

case class Defn_ExtensionGroup(
    /**
     * @type list: hydra/ext/scala/meta.Type.Param
     */
    tparams: Seq[Type_Param],
    
    /**
     * @type list:
     *         list: hydra/ext/scala/meta.Term.Param
     */
    paramss: Seq[Seq[Term_Param]],
    
    /**
     * @type hydra/ext/scala/meta.Stat
     */
    body: Stat
)

val _Defn_ExtensionGroup: String = "hydra/ext/scala/meta.Defn_ExtensionGroup"
val _Defn_ExtensionGroup_body: String = "body"
val _Defn_ExtensionGroup_paramss: String = "paramss"
val _Defn_ExtensionGroup_tparams: String = "tparams"
