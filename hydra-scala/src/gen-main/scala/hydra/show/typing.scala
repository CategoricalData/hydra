package hydra.show.typing

import hydra.core.*

import hydra.typing.*

import hydra.lib.lists

import hydra.lib.maps

import hydra.lib.pairs

import hydra.lib.strings

def typeConstraint(tc: hydra.typing.TypeConstraint): scala.Predef.String =
  {
  val ltyp: hydra.core.Type = (tc.left)
  val rtyp: hydra.core.Type = (tc.right)
  strings.cat(Seq(hydra.show.core.`type`(ltyp), "≡", hydra.show.core.`type`(rtyp)))
}

def typeSubst(ts: hydra.typing.TypeSubst): scala.Predef.String =
  {
  val subst: Map[hydra.core.Name, hydra.core.Type] = ts
  val pairs: Seq[Tuple2[hydra.core.Name, hydra.core.Type]] = maps.toList[hydra.core.Name, hydra.core.Type](subst)
  def showPair(pair: Tuple2[hydra.core.Name, hydra.core.Type]): scala.Predef.String =
    {
    val name: scala.Predef.String = pairs.first[hydra.core.Name, hydra.core.Type](pair)
    val typ: hydra.core.Type = pairs.second[hydra.core.Name, hydra.core.Type](pair)
    strings.cat(Seq(name, "↦", hydra.show.core.`type`(typ)))
  }
  val pairStrs: Seq[scala.Predef.String] = lists.map[Tuple2[hydra.core.Name, hydra.core.Type], scala.Predef.String](showPair)(pairs)
  strings.cat(Seq("{", strings.intercalate(",")(pairStrs), "}"))
}
