package hydra.ext.scala.meta

enum ImportExportStat:
    /**
     * @type hydra/ext/scala/meta.Import
     */
    case `import`(value: hydra.ext.scala.meta.Import) extends ImportExportStat
    /**
     * @type hydra/ext/scala/meta.Export
     */
    case `export`(value: hydra.ext.scala.meta.Export) extends ImportExportStat

val _ImportExportStat: String = "hydra/ext/scala/meta.ImportExportStat"
val _ImportExportStat_export: String = "export"
val _ImportExportStat_import: String = "import"
