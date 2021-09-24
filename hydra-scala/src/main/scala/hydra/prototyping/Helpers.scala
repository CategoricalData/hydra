package hydra.prototyping

import hydra.core.*


def functionType(dom: Type, cod: Type): Type = Type.function(FunctionType(dom, cod))

def nominalType(name: Name) = Type.nominal(name)

def stringTerm(s: String): Term = Term.atomic(AtomicValue.string(s))

val unitTerm: Term = Term.record(Seq())

def unitVariant(fname: FieldName): Term = variant(fname, unitTerm)

def variant(fname: FieldName, term: Term): Term = Term.union(Field(fname, term))
