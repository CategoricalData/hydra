package hydra.ext.scala.meta

case class Export(
    /**
     * @type list: hydra/ext/scala/meta.Importer
     */
    importers: Seq[Importer]
)

val _Export: String = "hydra/ext/scala/meta.Export"
val _Export_importers: String = "importers"
