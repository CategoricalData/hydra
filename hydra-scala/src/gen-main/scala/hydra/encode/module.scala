package hydra.encode.module

import hydra.core.*

import hydra.module.*

import hydra.lib.lists

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

def definition(v1: hydra.module.Definition): hydra.core.Term =
  v1 match
  case hydra.module.Definition.term(v_Definition_term_y) => hydra.core.Term.union(hydra.core.Injection("hydra.module.Definition",
     hydra.core.Field("term", hydra.encode.module.termDefinition(v_Definition_term_y))))
  case hydra.module.Definition.`type`(v_Definition_type_y) => hydra.core.Term.union(hydra.core.Injection("hydra.module.Definition",
     hydra.core.Field("type", hydra.encode.module.typeDefinition(v_Definition_type_y))))

def fileExtension(x: hydra.module.FileExtension): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.module.FileExtension", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def module(x: hydra.module.Module): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.module.Module", Seq(hydra.core.Field("namespace", hydra.encode.module.namespace(x.namespace)),
     hydra.core.Field("elements", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Term](hydra.encode.core.binding)(x.elements))),
     hydra.core.Field("termDependencies", hydra.core.Term.list(hydra.lib.lists.map[hydra.module.Namespace,
     hydra.core.Term](hydra.encode.module.namespace)(x.termDependencies))), hydra.core.Field("typeDependencies",
     hydra.core.Term.list(hydra.lib.lists.map[hydra.module.Namespace, hydra.core.Term](hydra.encode.module.namespace)(x.typeDependencies))),
     hydra.core.Field("description", hydra.core.Term.maybe(hydra.lib.maybes.map[scala.Predef.String, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.description))))))

def namespace(x: hydra.module.Namespace): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.module.Namespace", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def namespaces[T0](n: (T0 => hydra.core.Term))(x: hydra.module.Namespaces[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.module.Namespaces", Seq(hydra.core.Field("focus", hydra.core.Term.pair(hydra.lib.pairs.bimap[hydra.module.Namespace,
     T0, hydra.core.Term, hydra.core.Term](hydra.encode.module.namespace)(n)(x.focus))), hydra.core.Field("mapping",
     hydra.core.Term.map(hydra.lib.maps.bimap[hydra.module.Namespace, hydra.core.Term, T0, hydra.core.Term](hydra.encode.module.namespace)(n)(x.mapping))))))

def qualifiedName(x: hydra.module.QualifiedName): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.module.QualifiedName", Seq(hydra.core.Field("namespace",
     hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.module.Namespace, hydra.core.Term](hydra.encode.module.namespace)(x.namespace))),
     hydra.core.Field("local", hydra.core.Term.literal(hydra.core.Literal.string(x.local))))))

def termDefinition(x: hydra.module.TermDefinition): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.module.TermDefinition", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)), hydra.core.Field("term", hydra.encode.core.term(x.term)), hydra.core.Field("type",
     hydra.encode.core.typeScheme(x.`type`)))))

def typeDefinition(x: hydra.module.TypeDefinition): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.module.TypeDefinition", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)), hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)))))
