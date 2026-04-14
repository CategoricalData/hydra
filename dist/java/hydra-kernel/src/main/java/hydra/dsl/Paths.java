// Note: this is an automatically generated file. Do not edit.

package hydra.dsl;

/**
 * DSL functions for hydra.paths
 */
public interface Paths {
  static hydra.phantoms.TTerm<hydra.paths.SubtermEdge> subtermEdge(hydra.phantoms.TTerm<hydra.paths.SubtermNode> source, hydra.phantoms.TTerm<hydra.paths.SubtermPath> path, hydra.phantoms.TTerm<hydra.paths.SubtermNode> target) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), (source).value),
      new hydra.core.Field(new hydra.core.Name("path"), (path).value),
      new hydra.core.Field(new hydra.core.Name("target"), (target).value)))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermPath> subtermEdgePath(hydra.phantoms.TTerm<hydra.paths.SubtermEdge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermEdge"), new hydra.core.Name("path"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermNode> subtermEdgeSource(hydra.phantoms.TTerm<hydra.paths.SubtermEdge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermEdge"), new hydra.core.Name("source"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermNode> subtermEdgeTarget(hydra.phantoms.TTerm<hydra.paths.SubtermEdge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermEdge"), new hydra.core.Name("target"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermEdge> subtermEdgeWithPath(hydra.phantoms.TTerm<hydra.paths.SubtermEdge> original, hydra.phantoms.TTerm<hydra.paths.SubtermPath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermEdge"), new hydra.core.Name("source"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("path"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermEdge"), new hydra.core.Name("target"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermEdge> subtermEdgeWithSource(hydra.phantoms.TTerm<hydra.paths.SubtermEdge> original, hydra.phantoms.TTerm<hydra.paths.SubtermNode> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermEdge"), new hydra.core.Name("path"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermEdge"), new hydra.core.Name("target"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermEdge> subtermEdgeWithTarget(hydra.phantoms.TTerm<hydra.paths.SubtermEdge> original, hydra.phantoms.TTerm<hydra.paths.SubtermNode> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermEdge"), new hydra.core.Name("source"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermEdge"), new hydra.core.Name("path"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermGraph> subtermGraph(hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtermNode>> nodes, hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtermEdge>> edges) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermGraph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("nodes"), (nodes).value),
      new hydra.core.Field(new hydra.core.Name("edges"), (edges).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtermEdge>> subtermGraphEdges(hydra.phantoms.TTerm<hydra.paths.SubtermGraph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermGraph"), new hydra.core.Name("edges"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtermNode>> subtermGraphNodes(hydra.phantoms.TTerm<hydra.paths.SubtermGraph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermGraph"), new hydra.core.Name("nodes"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermGraph> subtermGraphWithEdges(hydra.phantoms.TTerm<hydra.paths.SubtermGraph> original, hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtermEdge>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermGraph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("nodes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermGraph"), new hydra.core.Name("nodes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("edges"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermGraph> subtermGraphWithNodes(hydra.phantoms.TTerm<hydra.paths.SubtermGraph> original, hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtermNode>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermGraph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("nodes"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("edges"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermGraph"), new hydra.core.Name("edges"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermNode> subtermNode(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<String> label, hydra.phantoms.TTerm<String> id) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("label"), (label).value),
      new hydra.core.Field(new hydra.core.Name("id"), (id).value)))));
  }

  static hydra.phantoms.TTerm<String> subtermNodeId(hydra.phantoms.TTerm<hydra.paths.SubtermNode> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermNode"), new hydra.core.Name("id"))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> subtermNodeLabel(hydra.phantoms.TTerm<hydra.paths.SubtermNode> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermNode"), new hydra.core.Name("label"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> subtermNodeName(hydra.phantoms.TTerm<hydra.paths.SubtermNode> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermNode"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermNode> subtermNodeWithId(hydra.phantoms.TTerm<hydra.paths.SubtermNode> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermNode"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermNode"), new hydra.core.Name("label"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("id"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermNode> subtermNodeWithLabel(hydra.phantoms.TTerm<hydra.paths.SubtermNode> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermNode"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("label"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("id"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermNode"), new hydra.core.Name("id"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermNode> subtermNodeWithName(hydra.phantoms.TTerm<hydra.paths.SubtermNode> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtermNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermNode"), new hydra.core.Name("label"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("id"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtermNode"), new hydra.core.Name("id"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermPath> subtermPath(hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtermStep>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.paths.SubtermPath"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepAnnotatedBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("annotatedBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepApplicationArgument() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("applicationArgument"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepApplicationFunction() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("applicationFunction"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepInjectionTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("injectionTerm"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepLambdaBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("lambdaBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepLetBinding(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("letBinding"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepLetBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("letBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepListElement(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("listElement"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepMapKey(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("mapKey"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepMapValue(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("mapValue"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepMaybeTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("maybeTerm"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepProductTerm(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("productTerm"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepRecordField(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("recordField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepSetElement(hydra.phantoms.TTerm<Integer> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("setElement"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepSumTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("sumTerm"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepTypeApplicationTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("typeApplicationTerm"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepTypeLambdaBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("typeLambdaBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepUnionCasesBranch(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("unionCasesBranch"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepUnionCasesDefault() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("unionCasesDefault"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtermStep> subtermStepWrappedTerm() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtermStep"), new hydra.core.Field(new hydra.core.Name("wrappedTerm"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> subtypeEdge(hydra.phantoms.TTerm<hydra.paths.SubtypeNode> source, hydra.phantoms.TTerm<hydra.paths.SubtypePath> path, hydra.phantoms.TTerm<hydra.paths.SubtypeNode> target) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), (source).value),
      new hydra.core.Field(new hydra.core.Name("path"), (path).value),
      new hydra.core.Field(new hydra.core.Name("target"), (target).value)))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypePath> subtypeEdgePath(hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeEdge"), new hydra.core.Name("path"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeNode> subtypeEdgeSource(hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeEdge"), new hydra.core.Name("source"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeNode> subtypeEdgeTarget(hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeEdge"), new hydra.core.Name("target"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> subtypeEdgeWithPath(hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> original, hydra.phantoms.TTerm<hydra.paths.SubtypePath> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeEdge"), new hydra.core.Name("source"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("path"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeEdge"), new hydra.core.Name("target"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> subtypeEdgeWithSource(hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> original, hydra.phantoms.TTerm<hydra.paths.SubtypeNode> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeEdge"), new hydra.core.Name("path"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeEdge"), new hydra.core.Name("target"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> subtypeEdgeWithTarget(hydra.phantoms.TTerm<hydra.paths.SubtypeEdge> original, hydra.phantoms.TTerm<hydra.paths.SubtypeNode> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeEdge"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("source"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeEdge"), new hydra.core.Name("source"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("path"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeEdge"), new hydra.core.Name("path"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("target"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeGraph> subtypeGraph(hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtypeNode>> nodes, hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtypeEdge>> edges) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeGraph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("nodes"), (nodes).value),
      new hydra.core.Field(new hydra.core.Name("edges"), (edges).value)))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtypeEdge>> subtypeGraphEdges(hydra.phantoms.TTerm<hydra.paths.SubtypeGraph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeGraph"), new hydra.core.Name("edges"))), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtypeNode>> subtypeGraphNodes(hydra.phantoms.TTerm<hydra.paths.SubtypeGraph> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeGraph"), new hydra.core.Name("nodes"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeGraph> subtypeGraphWithEdges(hydra.phantoms.TTerm<hydra.paths.SubtypeGraph> original, hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtypeEdge>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeGraph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("nodes"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeGraph"), new hydra.core.Name("nodes"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("edges"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeGraph> subtypeGraphWithNodes(hydra.phantoms.TTerm<hydra.paths.SubtypeGraph> original, hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtypeNode>> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeGraph"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("nodes"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("edges"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeGraph"), new hydra.core.Name("edges"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeNode> subtypeNode(hydra.phantoms.TTerm<hydra.core.Name> name, hydra.phantoms.TTerm<String> label, hydra.phantoms.TTerm<String> id) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (name).value),
      new hydra.core.Field(new hydra.core.Name("label"), (label).value),
      new hydra.core.Field(new hydra.core.Name("id"), (id).value)))));
  }

  static hydra.phantoms.TTerm<String> subtypeNodeId(hydra.phantoms.TTerm<hydra.paths.SubtypeNode> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeNode"), new hydra.core.Name("id"))), (x).value)));
  }

  static hydra.phantoms.TTerm<String> subtypeNodeLabel(hydra.phantoms.TTerm<hydra.paths.SubtypeNode> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeNode"), new hydra.core.Name("label"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.core.Name> subtypeNodeName(hydra.phantoms.TTerm<hydra.paths.SubtypeNode> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeNode"), new hydra.core.Name("name"))), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeNode> subtypeNodeWithId(hydra.phantoms.TTerm<hydra.paths.SubtypeNode> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeNode"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeNode"), new hydra.core.Name("label"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("id"), (newVal).value)))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeNode> subtypeNodeWithLabel(hydra.phantoms.TTerm<hydra.paths.SubtypeNode> original, hydra.phantoms.TTerm<String> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeNode"), new hydra.core.Name("name"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("label"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("id"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeNode"), new hydra.core.Name("id"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeNode> subtypeNodeWithName(hydra.phantoms.TTerm<hydra.paths.SubtypeNode> original, hydra.phantoms.TTerm<hydra.core.Name> newVal) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name("hydra.paths.SubtypeNode"), java.util.Arrays.asList(
      new hydra.core.Field(new hydra.core.Name("name"), (newVal).value),
      new hydra.core.Field(new hydra.core.Name("label"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeNode"), new hydra.core.Name("label"))), (original).value))),
      new hydra.core.Field(new hydra.core.Name("id"), new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Project(new hydra.core.Projection(new hydra.core.Name("hydra.paths.SubtypeNode"), new hydra.core.Name("id"))), (original).value)))))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypePath> subtypePath(hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtypeStep>> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Wrap(new hydra.core.WrappedTerm(new hydra.core.Name("hydra.paths.SubtypePath"), (x).value)));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepAnnotatedBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("annotatedBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepApplicationArgument() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("applicationArgument"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepApplicationFunction() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("applicationFunction"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepEitherLeft() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("eitherLeft"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepEitherRight() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("eitherRight"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepForallBody() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("forallBody"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepFunctionCodomain() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("functionCodomain"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepFunctionDomain() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("functionDomain"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepListElement() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("listElement"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepMapKeys() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("mapKeys"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepMapValues() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("mapValues"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepMaybeElement() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("maybeElement"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepPairFirst() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("pairFirst"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepPairSecond() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("pairSecond"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepRecordField(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("recordField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepSetElement() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("setElement"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepUnionField(hydra.phantoms.TTerm<hydra.core.Name> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("unionField"), (x).value))));
  }

  static hydra.phantoms.TTerm<hydra.paths.SubtypeStep> subtypeStepWrappedType() {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Inject(new hydra.core.Injection(new hydra.core.Name("hydra.paths.SubtypeStep"), new hydra.core.Field(new hydra.core.Name("wrappedType"), new hydra.core.Term.Unit()))));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtermStep>> unSubtermPath(hydra.phantoms.TTerm<hydra.paths.SubtermPath> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.paths.SubtermPath")), (x).value)));
  }

  static hydra.phantoms.TTerm<java.util.List<hydra.paths.SubtypeStep>> unSubtypePath(hydra.phantoms.TTerm<hydra.paths.SubtypePath> x) {
    return new hydra.phantoms.TTerm(new hydra.core.Term.Application(new hydra.core.Application(new hydra.core.Term.Unwrap(new hydra.core.Name("hydra.paths.SubtypePath")), (x).value)));
  }
}
