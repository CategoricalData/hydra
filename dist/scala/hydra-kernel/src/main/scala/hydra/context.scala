package hydra.context

import hydra.core.*

import hydra.core

case class Context(trace: Seq[scala.Predef.String], messages: Seq[scala.Predef.String],
   other: Map[hydra.core.Name, hydra.core.Term])

case class InContext[E](`object`: E, context: hydra.context.Context)
