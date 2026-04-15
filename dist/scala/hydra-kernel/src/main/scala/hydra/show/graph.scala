package hydra.show.graph

def graph(elements: Seq[hydra.core.Binding]): scala.Predef.String =
  {
  lazy val elementStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Binding,
     scala.Predef.String](hydra.show.core.binding)(elements)
  hydra.lib.strings.cat(Seq("{", hydra.lib.strings.intercalate(", ")(elementStrs), "}"))
}
