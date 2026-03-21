package hydra.show.graph

import hydra.lib.lists

import hydra.lib.strings

def graph(elements: Seq[hydra.core.Binding]): scala.Predef.String =
  {
  val elementStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[hydra.core.Binding, scala.Predef.String](hydra.show.core.binding)(elements)
  hydra.lib.strings.cat(Seq("{", hydra.lib.strings.intercalate(", ")(elementStrs), "}"))
}
