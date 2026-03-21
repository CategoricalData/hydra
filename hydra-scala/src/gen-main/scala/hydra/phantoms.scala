package hydra.phantoms

import hydra.core.*

import hydra.core

case class TBinding[A](name: hydra.core.Name, term: hydra.phantoms.TTerm[A])

type TTerm [A] = hydra.core.Term
