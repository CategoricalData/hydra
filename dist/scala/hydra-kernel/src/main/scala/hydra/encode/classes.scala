package hydra.encode.classes

import hydra.classes.*

import hydra.core.*

def typeClass(v1: hydra.classes.TypeClass): hydra.core.Term =
  v1 match
  case hydra.classes.TypeClass.equality => hydra.core.Term.inject(hydra.core.Injection("hydra.classes.TypeClass",
     hydra.core.Field("equality", hydra.core.Term.unit)))
  case hydra.classes.TypeClass.ordering => hydra.core.Term.inject(hydra.core.Injection("hydra.classes.TypeClass",
     hydra.core.Field("ordering", hydra.core.Term.unit)))
