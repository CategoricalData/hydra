package hydra.encode.paths

import hydra.core.*

import hydra.paths.*

def subtermEdge(x: hydra.paths.SubtermEdge): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.paths.SubtermEdge", Seq(hydra.core.Field("source", hydra.encode.paths.subtermNode(x.source)),
     hydra.core.Field("path", hydra.encode.paths.subtermPath(x.path)), hydra.core.Field("target", hydra.encode.paths.subtermNode(x.target)))))

def subtermGraph(x: hydra.paths.SubtermGraph): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.paths.SubtermGraph", Seq(hydra.core.Field("nodes", hydra.core.Term.list(hydra.lib.lists.map[hydra.paths.SubtermNode,
     hydra.core.Term](hydra.encode.paths.subtermNode)(x.nodes))), hydra.core.Field("edges", hydra.core.Term.list(hydra.lib.lists.map[hydra.paths.SubtermEdge,
     hydra.core.Term](hydra.encode.paths.subtermEdge)(x.edges))))))

def subtermNode(x: hydra.paths.SubtermNode): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.paths.SubtermNode", Seq(hydra.core.Field("name", hydra.encode.core.name(x.name)),
     hydra.core.Field("label", hydra.core.Term.literal(hydra.core.Literal.string(x.label))), hydra.core.Field("id",
     hydra.core.Term.literal(hydra.core.Literal.string(x.id))))))

def subtermPath(x: hydra.paths.SubtermPath): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.paths.SubtermPath", hydra.core.Term.list(hydra.lib.lists.map[hydra.paths.SubtermStep,
     hydra.core.Term](hydra.encode.paths.subtermStep)(x))))

def subtermStep(v1: hydra.paths.SubtermStep): hydra.core.Term =
  v1 match
  case hydra.paths.SubtermStep.annotatedBody => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("annotatedBody", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.applicationFunction => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("applicationFunction", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.applicationArgument => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("applicationArgument", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.lambdaBody => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("lambdaBody", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.unionCasesDefault => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("unionCasesDefault", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.unionCasesBranch(v_SubtermStep_unionCasesBranch_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("unionCasesBranch", hydra.encode.core.name(v_SubtermStep_unionCasesBranch_y))))
  case hydra.paths.SubtermStep.letBody => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("letBody", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.letBinding(v_SubtermStep_letBinding_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("letBinding", hydra.encode.core.name(v_SubtermStep_letBinding_y))))
  case hydra.paths.SubtermStep.listElement(v_SubtermStep_listElement_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("listElement", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_SubtermStep_listElement_y))))))
  case hydra.paths.SubtermStep.mapKey(v_SubtermStep_mapKey_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("mapKey", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_SubtermStep_mapKey_y))))))
  case hydra.paths.SubtermStep.mapValue(v_SubtermStep_mapValue_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("mapValue", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_SubtermStep_mapValue_y))))))
  case hydra.paths.SubtermStep.maybeTerm => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("maybeTerm", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.productTerm(v_SubtermStep_productTerm_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("productTerm", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_SubtermStep_productTerm_y))))))
  case hydra.paths.SubtermStep.recordField(v_SubtermStep_recordField_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("recordField", hydra.encode.core.name(v_SubtermStep_recordField_y))))
  case hydra.paths.SubtermStep.setElement(v_SubtermStep_setElement_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("setElement", hydra.core.Term.literal(hydra.core.Literal.integer(hydra.core.IntegerValue.int32(v_SubtermStep_setElement_y))))))
  case hydra.paths.SubtermStep.sumTerm => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("sumTerm", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.typeLambdaBody => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("typeLambdaBody", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.typeApplicationTerm => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("typeApplicationTerm", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.injectionTerm => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("injectionTerm", hydra.core.Term.unit)))
  case hydra.paths.SubtermStep.wrappedTerm => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtermStep",
     hydra.core.Field("wrappedTerm", hydra.core.Term.unit)))

def subtypeEdge(x: hydra.paths.SubtypeEdge): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.paths.SubtypeEdge", Seq(hydra.core.Field("source", hydra.encode.paths.subtypeNode(x.source)),
     hydra.core.Field("path", hydra.encode.paths.subtypePath(x.path)), hydra.core.Field("target", hydra.encode.paths.subtypeNode(x.target)))))

def subtypeGraph(x: hydra.paths.SubtypeGraph): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.paths.SubtypeGraph", Seq(hydra.core.Field("nodes", hydra.core.Term.list(hydra.lib.lists.map[hydra.paths.SubtypeNode,
     hydra.core.Term](hydra.encode.paths.subtypeNode)(x.nodes))), hydra.core.Field("edges", hydra.core.Term.list(hydra.lib.lists.map[hydra.paths.SubtypeEdge,
     hydra.core.Term](hydra.encode.paths.subtypeEdge)(x.edges))))))

def subtypeNode(x: hydra.paths.SubtypeNode): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.paths.SubtypeNode", Seq(hydra.core.Field("name", hydra.encode.core.name(x.name)),
     hydra.core.Field("label", hydra.core.Term.literal(hydra.core.Literal.string(x.label))), hydra.core.Field("id",
     hydra.core.Term.literal(hydra.core.Literal.string(x.id))))))

def subtypePath(x: hydra.paths.SubtypePath): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.paths.SubtypePath", hydra.core.Term.list(hydra.lib.lists.map[hydra.paths.SubtypeStep,
     hydra.core.Term](hydra.encode.paths.subtypeStep)(x))))

def subtypeStep(v1: hydra.paths.SubtypeStep): hydra.core.Term =
  v1 match
  case hydra.paths.SubtypeStep.annotatedBody => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("annotatedBody", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.applicationFunction => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("applicationFunction", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.applicationArgument => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("applicationArgument", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.eitherLeft => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("eitherLeft", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.eitherRight => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("eitherRight", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.forallBody => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("forallBody", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.functionDomain => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("functionDomain", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.functionCodomain => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("functionCodomain", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.listElement => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("listElement", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.mapKeys => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("mapKeys", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.mapValues => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("mapValues", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.maybeElement => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("maybeElement", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.pairFirst => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("pairFirst", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.pairSecond => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("pairSecond", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.recordField(v_SubtypeStep_recordField_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("recordField", hydra.encode.core.name(v_SubtypeStep_recordField_y))))
  case hydra.paths.SubtypeStep.setElement => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("setElement", hydra.core.Term.unit)))
  case hydra.paths.SubtypeStep.unionField(v_SubtypeStep_unionField_y) => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("unionField", hydra.encode.core.name(v_SubtypeStep_unionField_y))))
  case hydra.paths.SubtypeStep.wrappedType => hydra.core.Term.union(hydra.core.Injection("hydra.paths.SubtypeStep",
     hydra.core.Field("wrappedType", hydra.core.Term.unit)))
