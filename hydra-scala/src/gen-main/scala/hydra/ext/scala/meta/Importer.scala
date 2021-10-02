package hydra.ext.scala.meta

case class Importer(
    /**
     * @type hydra/ext/scala/meta.Term.Ref
     */
    ref: Term_Ref,
    
    /**
     * @type list: hydra/ext/scala/meta.Importee
     */
    importees: Seq[Importee]
)

val _Importer: String = "hydra/ext/scala/meta.Importer"
val _Importer_importees: String = "importees"
val _Importer_ref: String = "ref"
