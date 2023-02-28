package hydra.graph;

/**
 * A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph
 */
public class Graph<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/graph.Graph");
  
  /**
   * All of the elements in the graph
   */
  public final java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements;
  
  /**
   * The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)
   */
  public final java.util.Map<hydra.core.Name, java.util.Optional<hydra.core.Term<M>>> environment;
  
  /**
   * The body of the term which generated this context
   */
  public final hydra.core.Term<M> body;
  
  /**
   * All supported primitive constants and functions, by name
   */
  public final java.util.Map<hydra.core.Name, hydra.graph.Primitive<M>> primitives;
  
  /**
   * The annotation class which is supported in this context
   */
  public final hydra.graph.AnnotationClass<M> annotations;
  
  /**
   * The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph.
   */
  public final java.util.Optional<hydra.graph.Graph<M>> schema;
  
  public Graph (java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements, java.util.Map<hydra.core.Name, java.util.Optional<hydra.core.Term<M>>> environment, hydra.core.Term<M> body, java.util.Map<hydra.core.Name, hydra.graph.Primitive<M>> primitives, hydra.graph.AnnotationClass<M> annotations, java.util.Optional<hydra.graph.Graph<M>> schema) {
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
  
  public Graph withElements(java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements) {
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withEnvironment(java.util.Map<hydra.core.Name, java.util.Optional<hydra.core.Term<M>>> environment) {
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withBody(hydra.core.Term<M> body) {
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withPrimitives(java.util.Map<hydra.core.Name, hydra.graph.Primitive<M>> primitives) {
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withAnnotations(hydra.graph.AnnotationClass<M> annotations) {
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
  
  public Graph withSchema(java.util.Optional<hydra.graph.Graph<M>> schema) {
    return new Graph(elements, environment, body, primitives, annotations, schema);
  }
}