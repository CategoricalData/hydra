// Note: this is an automatically generated file. Do not edit.

package hydra.graph;

/**
 * A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph
 */
public class Graph<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/graph.Graph");
  
  /**
   * All of the elements in the graph
   */
  public final java.util.Map<hydra.core.Name, hydra.graph.Element<A>> elements;
  
  /**
   * The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)
   */
  public final java.util.Map<hydra.core.Name, hydra.util.Opt<hydra.core.Term<A>>> environment;
  
  /**
   * The body of the term which generated this context
   */
  public final hydra.core.Term<A> body;
  
  /**
   * All supported primitive constants and functions, by name
   */
  public final java.util.Map<hydra.core.Name, hydra.graph.Primitive<A>> primitives;
  
  /**
   * The annotation class which is supported in this context
   */
  public final hydra.graph.AnnotationClass<A> annotations;
  
  /**
   * The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph.
   */
  public final hydra.util.Opt<hydra.graph.Graph<A>> schema;
  
  public Graph (java.util.Map<hydra.core.Name, hydra.graph.Element<A>> elements, java.util.Map<hydra.core.Name, hydra.util.Opt<hydra.core.Term<A>>> environment, hydra.core.Term<A> body, java.util.Map<hydra.core.Name, hydra.graph.Primitive<A>> primitives, hydra.graph.AnnotationClass<A> annotations, hydra.util.Opt<hydra.graph.Graph<A>> schema) {
    java.util.Objects.requireNonNull((elements));
    java.util.Objects.requireNonNull((environment));
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((primitives));
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((schema));
    this.elements = elements;
    this.environment = environment;
    this.body = body;
    this.primitives = primitives;
    this.annotations = annotations;
    this.schema = schema;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) (other);
    return elements.equals(o.elements) && environment.equals(o.environment) && body.equals(o.body) && primitives.equals(o.primitives) && annotations.equals(o.annotations) && schema.equals(o.schema);
  }
  
  @Override
  public int hashCode() {
    return 2 * elements.hashCode() + 3 * environment.hashCode() + 5 * body.hashCode() + 7 * primitives.hashCode() + 11 * annotations.hashCode() + 13 * schema.hashCode();
  }
  
  public Graph withElements(java.util.Map<hydra.core.Name, hydra.graph.Element<A>> elements) {
    java.util.Objects.requireNonNull((elements));
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withEnvironment(java.util.Map<hydra.core.Name, hydra.util.Opt<hydra.core.Term<A>>> environment) {
    java.util.Objects.requireNonNull((environment));
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withBody(hydra.core.Term<A> body) {
    java.util.Objects.requireNonNull((body));
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withPrimitives(java.util.Map<hydra.core.Name, hydra.graph.Primitive<A>> primitives) {
    java.util.Objects.requireNonNull((primitives));
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withAnnotations(hydra.graph.AnnotationClass<A> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withSchema(hydra.util.Opt<hydra.graph.Graph<A>> schema) {
    java.util.Objects.requireNonNull((schema));
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
}