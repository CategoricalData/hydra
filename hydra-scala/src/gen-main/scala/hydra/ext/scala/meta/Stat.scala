package hydra.ext.scala.meta

enum Stat:
    /**
     * @type hydra/ext/scala/meta.Term
     */
    case term(value: Term) extends Stat
    /**
     * @type hydra/ext/scala/meta.Decl
     */
    case decl(value: Decl) extends Stat
    /**
     * @type hydra/ext/scala/meta.Defn
     */
    case defn(value: Defn) extends Stat
    /**
     * @type hydra/ext/scala/meta.ImportExportStat
     */
    case importExportStat(value: ImportExportStat) extends Stat

val _Stat: String = "hydra/ext/scala/meta.Stat"
val _Stat_decl: String = "decl"
val _Stat_defn: String = "defn"
val _Stat_importExportStat: String = "importExportStat"
val _Stat_term: String = "term"
