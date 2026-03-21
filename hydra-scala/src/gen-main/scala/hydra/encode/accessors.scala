package hydra.encode.accessors

import hydra.accessors.*

import hydra.core.*

import hydra.lib.lists

def accessorEdge(x: hydra.accessors.AccessorEdge): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.accessors.AccessorEdge", Seq(hydra.core.Field("source",
     hydra.encode.accessors.accessorNode(x.source)), hydra.core.Field("path", hydra.encode.accessors.accessorPath(x.path)),
     hydra.core.Field("target", hydra.encode.accessors.accessorNode(x.target)))))

def accessorGraph(x: hydra.accessors.AccessorGraph): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.accessors.AccessorGraph", Seq(hydra.core.Field("nodes",
     hydra.core.Term.list(hydra.lib.lists.map[hydra.accessors.AccessorNode, hydra.core.Term](hydra.encode.accessors.accessorNode)(x.nodes))),
     hydra.core.Field("edges", hydra.core.Term.list(hydra.lib.lists.map[hydra.accessors.AccessorEdge,
     hydra.core.Term](hydra.encode.accessors.accessorEdge)(x.edges))))))

def accessorNode(x: hydra.accessors.AccessorNode): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.accessors.AccessorNode", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)), hydra.core.Field("label", hydra.core.Term.literal(hydra.core.Literal.string(x.label))),
     hydra.core.Field("id", hydra.core.Term.literal(hydra.core.Literal.string(x.id))))))

def accessorPath(x: hydra.accessors.AccessorPath): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.accessors.AccessorPath", hydra.core.Term.list(hydra.lib.lists.map[hydra.accessors.TermAccessor,
     hydra.core.Term](hydra.encode.accessors.termAccessor)(x))))

def termAccessor(v1: hydra.accessors.TermAccessor): hydra.core.Term =
  v1 match
  case hydra.accessors.TermAccessor.annotatedBody => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("annotatedBody", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.applicationFunction => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("applicationFunction", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.applicationArgument => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("applicationArgument", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.lambdaBody => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("lambdaBody", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.unionCasesDefault => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("unionCasesDefault", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.unionCasesBranch(v_TermAccessor_unionCasesBranch_y) => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("unionCasesBranch", hydra.encode.core.name(v_TermAccessor_unionCasesBranch_y))))
  case hydra.accessors.TermAccessor.letBody => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("letBody", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.letBinding(v_TermAccessor_letBinding_y) => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("letBinding", hydra.encode.core.name(v_TermAccessor_letBinding_y))))
  case hydra.accessors.TermAccessor.listElement(v_TermAccessor_listElement_y) => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("listElement", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_TermAccessor_listElement_y))))))
  case hydra.accessors.TermAccessor.mapKey(v_TermAccessor_mapKey_y) => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("mapKey", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_TermAccessor_mapKey_y))))))
  case hydra.accessors.TermAccessor.mapValue(v_TermAccessor_mapValue_y) => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("mapValue", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_TermAccessor_mapValue_y))))))
  case hydra.accessors.TermAccessor.maybeTerm => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("maybeTerm", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.productTerm(v_TermAccessor_productTerm_y) => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("productTerm", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_TermAccessor_productTerm_y))))))
  case hydra.accessors.TermAccessor.recordField(v_TermAccessor_recordField_y) => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("recordField", hydra.encode.core.name(v_TermAccessor_recordField_y))))
  case hydra.accessors.TermAccessor.setElement(v_TermAccessor_setElement_y) => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("setElement", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_TermAccessor_setElement_y))))))
  case hydra.accessors.TermAccessor.sumTerm => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("sumTerm", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.typeLambdaBody => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("typeLambdaBody", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.typeApplicationTerm => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("typeApplicationTerm", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.injectionTerm => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("injectionTerm", hydra.core.Term.unit)))
  case hydra.accessors.TermAccessor.wrappedTerm => hydra.core.Term.union(hydra.core.Injection("hydra.accessors.TermAccessor",
     hydra.core.Field("wrappedTerm", hydra.core.Term.unit)))
