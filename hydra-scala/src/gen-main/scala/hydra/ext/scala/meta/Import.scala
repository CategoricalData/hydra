package hydra.ext.scala.meta

case class Import (
    /**
     * @type list: hydra/ext/scala/meta.Importer
     */
    importers: Seq[hydra.ext.scala.meta.Importer]
)

val _Import: String = "hydra/ext/scala/meta.Import"
val _Import_importers: String = "importers"
