package hydra.encode.packaging

import hydra.core.*

import hydra.packaging.*

import hydra.lib.lists

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

def definition(v1: hydra.packaging.Definition): hydra.core.Term =
  v1 match
  case hydra.packaging.Definition.term(v_Definition_term_y) => hydra.core.Term.union(hydra.core.Injection("hydra.packaging.Definition",
     hydra.core.Field("term", hydra.encode.packaging.termDefinition(v_Definition_term_y))))
  case hydra.packaging.Definition.`type`(v_Definition_type_y) => hydra.core.Term.union(hydra.core.Injection("hydra.packaging.Definition",
     hydra.core.Field("type", hydra.encode.packaging.typeDefinition(v_Definition_type_y))))

def fileExtension(x: hydra.packaging.FileExtension): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.packaging.FileExtension", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def module(x: hydra.packaging.Module): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.packaging.Module", Seq(hydra.core.Field("namespace",
     hydra.encode.packaging.namespace(x.namespace)), hydra.core.Field("definitions", hydra.core.Term.list(hydra.lib.lists.map[hydra.packaging.Definition,
     hydra.core.Term](hydra.encode.packaging.definition)(x.definitions))), hydra.core.Field("termDependencies",
     hydra.core.Term.list(hydra.lib.lists.map[hydra.packaging.Namespace, hydra.core.Term](hydra.encode.packaging.namespace)(x.termDependencies))),
     hydra.core.Field("typeDependencies", hydra.core.Term.list(hydra.lib.lists.map[hydra.packaging.Namespace,
     hydra.core.Term](hydra.encode.packaging.namespace)(x.typeDependencies))), hydra.core.Field("description",
     hydra.core.Term.maybe(hydra.lib.maybes.map[scala.Predef.String, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.description))))))

def namespace(x: hydra.packaging.Namespace): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.packaging.Namespace", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def namespaces[T0](n: (T0 => hydra.core.Term))(x: hydra.packaging.Namespaces[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.packaging.Namespaces", Seq(hydra.core.Field("focus",
     hydra.core.Term.pair(hydra.lib.pairs.bimap[hydra.packaging.Namespace, T0, hydra.core.Term, hydra.core.Term](hydra.encode.packaging.namespace)(n)(x.focus))),
     hydra.core.Field("mapping", hydra.core.Term.map(hydra.lib.maps.bimap[hydra.packaging.Namespace, hydra.core.Term,
     T0, hydra.core.Term](hydra.encode.packaging.namespace)(n)(x.mapping))))))

def `package`(x: hydra.packaging.Package): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.packaging.Package", Seq(hydra.core.Field("name", hydra.encode.packaging.packageName(x.name)),
     hydra.core.Field("modules", hydra.core.Term.list(hydra.lib.lists.map[hydra.packaging.Module, hydra.core.Term](hydra.encode.packaging.module)(x.modules))),
     hydra.core.Field("dependencies", hydra.core.Term.list(hydra.lib.lists.map[hydra.packaging.PackageName,
     hydra.core.Term](hydra.encode.packaging.packageName)(x.dependencies))), hydra.core.Field("description",
     hydra.core.Term.maybe(hydra.lib.maybes.map[scala.Predef.String, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.description))))))

def packageName(x: hydra.packaging.PackageName): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.packaging.PackageName", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def qualifiedName(x: hydra.packaging.QualifiedName): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.packaging.QualifiedName", Seq(hydra.core.Field("namespace",
     hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.packaging.Namespace, hydra.core.Term](hydra.encode.packaging.namespace)(x.namespace))),
     hydra.core.Field("local", hydra.core.Term.literal(hydra.core.Literal.string(x.local))))))

def termDefinition(x: hydra.packaging.TermDefinition): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.packaging.TermDefinition", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)), hydra.core.Field("term", hydra.encode.core.term(x.term)), hydra.core.Field("type",
     hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.TypeScheme, hydra.core.Term](hydra.encode.core.typeScheme)(x.`type`))))))

def typeDefinition(x: hydra.packaging.TypeDefinition): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.packaging.TypeDefinition", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)), hydra.core.Field("type", hydra.encode.core.typeScheme(x.`type`)))))
