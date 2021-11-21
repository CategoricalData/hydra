package hydra.impl.scala

import hydra.core.*;


case class Meta ()

def functionType(dom: Type, cod: Type): Type = Type.function(FunctionType(dom, cod))

def list[a](meta: a, els: Seq[Term[a]]): Term[a] = Term(Expression.list(els), meta)

def nominalType(name: Name) = Type.nominal(name)

def recordTerm[a](meta: a, fields: Seq[Field[a]]): Term[a] = Term(Expression.record(fields), meta)

def stringTerm[a](meta: a, s: String): Term[a] = Term(Expression.atomic(AtomicValue.string(s)), meta)

def unitTerm[a](meta: a): Term[a] = Term(Expression.record(Seq()), meta)

def unitVariant[a](meta: a, fname: FieldName): Term[a] = variant(meta, fname, unitTerm(meta))

def variant[a](meta: a, fname: FieldName, term: Term[a]): Term[a] = Term(Expression.union(Field(fname, term)), meta)
