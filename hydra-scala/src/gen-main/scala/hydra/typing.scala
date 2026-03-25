package hydra.typing

import hydra.context.*

import hydra.core.*

import hydra.context

import hydra.core

case class FunctionStructure[Env](typeParams: Seq[hydra.core.Name], params: Seq[hydra.core.Name], bindings: Seq[hydra.core.Binding],
   body: hydra.core.Term, domains: Seq[hydra.core.Type], codomain: Option[hydra.core.Type], environment: Env)

case class InferenceResult(term: hydra.core.Term, `type`: hydra.core.Type, subst: hydra.typing.TypeSubst,
   classConstraints: Map[hydra.core.Name, hydra.core.TypeVariableMetadata], context: hydra.context.Context)

type TermSubst = Map[hydra.core.Name, hydra.core.Term]

case class TypeConstraint(left: hydra.core.Type, right: hydra.core.Type, comment: scala.Predef.String)

type TypeSubst = Map[hydra.core.Name, hydra.core.Type]
