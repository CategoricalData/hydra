package hydra.show.graph

import hydra.lib.lists

import hydra.lib.strings

def graph(elements: Seq[hydra.core.Binding]): scala.Predef.String =
  {
  val elementStrs: Seq[scala.Predef.String] = lists.map[hydra.core.Binding, scala.Predef.String](hydra.show.core.binding)(elements)
  strings.cat(Seq("{", strings.intercalate(", ")(elementStrs), "}"))
}
