// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.accessors
 */
public interface Accessors {
  static hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> accessorEdge(hydra.phantoms.TTerm<hydra.accessors.AccessorNode> source, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> path, hydra.phantoms.TTerm<hydra.accessors.AccessorNode> target) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("source"), (source).value),
      new hydra.core.Field(new hydra.core.Name("path"), (path).value),
      new hydra.core.Field(new hydra.core.Name("target"), (target).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorNode> accessorEdgeSource(hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorEdge"), new hydra.core.Name("source"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> accessorEdgePath(hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorEdge"), new hydra.core.Name("path"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorNode> accessorEdgeTarget(hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorEdge"), new hydra.core.Name("target"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> accessorEdgeWithSource(hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> original, hydra.phantoms.TTerm<hydra.accessors.AccessorNode> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("source"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorEdge"), new hydra.core.Name("path"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorEdge"), new hydra.core.Name("target"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> accessorEdgeWithPath(hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> original, hydra.phantoms.TTerm<hydra.accessors.AccessorPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorEdge"), new hydra.core.Name("source"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("path"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorEdge"), new hydra.core.Name("target"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> accessorEdgeWithTarget(hydra.phantoms.TTerm<hydra.accessors.AccessorEdge> original, hydra.phantoms.TTerm<hydra.accessors.AccessorNode> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorEdge"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorEdge"), new hydra.core.Name("source"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorEdge"), new hydra.core.Name("path"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorGraph> accessorGraph(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.accessors.AccessorNode>> nodes, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.accessors.AccessorEdge>> edges) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorGraph"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("nodes"), (nodes).value),
      new hydra.core.Field(new hydra.core.Name("edges"), (edges).value)))));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.accessors.AccessorNode>> accessorGraphNodes(hydra.phantoms.TTerm<hydra.accessors.AccessorGraph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorGraph"), new hydra.core.Name("nodes"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.accessors.AccessorEdge>> accessorGraphEdges(hydra.phantoms.TTerm<hydra.accessors.AccessorGraph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorGraph"), new hydra.core.Name("edges"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorGraph> accessorGraphWithNodes(hydra.phantoms.TTerm<hydra.accessors.AccessorGraph> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.accessors.AccessorNode>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorGraph"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("nodes"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("edges"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorGraph"), new hydra.core.Name("edges"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorGraph> accessorGraphWithEdges(hydra.phantoms.TTerm<hydra.accessors.AccessorGraph> original, hydra.phantoms.TTerm<hydra.util.ConsList<hydra.accessors.AccessorEdge>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorGraph"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("nodes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorGraph"), new hydra.core.Name("nodes"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("edges"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorNode> accessorNode(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<String> label, hydra.phantoms.TTerm<String> id) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorNode"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("label"), (label).value),
      new hydra.core.Field(new hydra.core.Name("id"), (id).value)))));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> accessorNodeName(hydra.phantoms.TTerm<hydra.accessors.AccessorNode> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorNode"), new hydra.core.Name("name"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> accessorNodeLabel(hydra.phantoms.TTerm<hydra.accessors.AccessorNode> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorNode"), new hydra.core.Name("label"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> accessorNodeId(hydra.phantoms.TTerm<hydra.accessors.AccessorNode> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorNode"), new hydra.core.Name("id"))))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorNode> accessorNodeWithName(hydra.phantoms.TTerm<hydra.accessors.AccessorNode> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorNode"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorNode"), new hydra.core.Name("label"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("id"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorNode"), new hydra.core.Name("id"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorNode> accessorNodeWithLabel(hydra.phantoms.TTerm<hydra.accessors.AccessorNode> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorNode"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorNode"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("label"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("id"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorNode"), new hydra.core.Name("id"))))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorNode> accessorNodeWithId(hydra.phantoms.TTerm<hydra.accessors.AccessorNode> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.accessors.AccessorNode"), hydra.util.ConsList.of(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorNode"), new hydra.core.Name("name"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Record(new hydra.core.Projection(new hydra.core.Name("hydra.accessors.AccessorNode"), new hydra.core.Name("label"))))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("id"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.AccessorPath> accessorPath(hydra.phantoms.TTerm<hydra.util.ConsList<hydra.accessors.TermAccessor>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.accessors.AccessorPath"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.util.ConsList<hydra.accessors.TermAccessor>> unAccessorPath(hydra.phantoms.TTerm<hydra.accessors.AccessorPath> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Function(new hydra.core.Function.Elimination(new hydra.core.Elimination.Wrap(new hydra.core.Name("hydra.accessors.AccessorPath")))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorAnnotatedBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("annotatedBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorApplicationFunction() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("applicationFunction"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorApplicationArgument() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("applicationArgument"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorLambdaBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("lambdaBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorUnionCasesDefault() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("unionCasesDefault"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorUnionCasesBranch(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("unionCasesBranch"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorLetBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("letBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorLetBinding(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("letBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorListElement(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("listElement"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorMapKey(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("mapKey"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorMapValue(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("mapValue"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorMaybeTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("maybeTerm"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorProductTerm(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("productTerm"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorRecordField(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("recordField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorSetElement(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("setElement"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorSumTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("sumTerm"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorTypeLambdaBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("typeLambdaBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorTypeApplicationTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("typeApplicationTerm"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorInjectionTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("injectionTerm"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.accessors.TermAccessor> termAccessorWrappedTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name("hydra.accessors.TermAccessor"), new hydra.core.Field(new hydra.core.Name("wrappedTerm"), new hydra.core.Term.Unit()))));
  }
}
