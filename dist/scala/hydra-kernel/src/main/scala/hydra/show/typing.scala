package hydra.show.typing

import hydra.core.*

import hydra.typing.*

def typeConstraint(tc: hydra.typing.TypeConstraint): scala.Predef.String =
  {
  lazy val ltyp: hydra.core.Type = (tc.left)
  lazy val rtyp: hydra.core.Type = (tc.right)
  hydra.lib.strings.cat(Seq(hydra.show.core.`type`(ltyp), "\u2261", hydra.show.core.`type`(rtyp)))
}

def typeSubst(ts: hydra.typing.TypeSubst): scala.Predef.String =
  {
  lazy val subst: Map[hydra.core.Name, hydra.core.Type] = ts
  lazy val pairs: Seq[Tuple2[hydra.core.Name, hydra.core.Type]] = hydra.lib.maps.toList[hydra.core.Name, hydra.core.Type](subst)
  def showPair(pair: Tuple2[hydra.core.Name, hydra.core.Type]): scala.Predef.String =
    {
    lazy val name: scala.Predef.String = hydra.lib.pairs.first[hydra.core.Name, hydra.core.Type](pair)
    lazy val typ: hydra.core.Type = hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](pair)
    hydra.lib.strings.cat(Seq(name, "\u21A6", hydra.show.core.`type`(typ)))
  }
  lazy val pairStrs: Seq[scala.Predef.String] = hydra.lib.lists.map[Tuple2[hydra.core.Name, hydra.core.Type], scala.Predef.String](showPair)(pairs)
  hydra.lib.strings.cat(Seq("{", hydra.lib.strings.intercalate(",")(pairStrs), "}"))
}
