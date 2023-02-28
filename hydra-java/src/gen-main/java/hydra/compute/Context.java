package hydra.compute;

/**
 * An environment containing a graph together with primitive functions and other necessary components for evaluation
 */
public class Context<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.Context");
  
  /**
   * The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)
   */
  public final java.util.Map<hydra.core.Name, java.util.Optional<hydra.core.Term<M>>> environment;
  
  /**
   * The body of the term which generated this context
   */
  public final hydra.core.Term<M> body;
  
  /**
   * The graph itself
   */
  public final hydra.graph.Graph<M> graph;
  
  /**
   * All supported primitive constants and functions, by name
   */
  public final java.util.Map<hydra.core.Name, hydra.compute.Primitive<M>> primitives;
  
  /**
   * The annotation class which is supported in this context
   */
  public final hydra.compute.AnnotationClass<M> annotations;
  
  /**
   * The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph.
   */
  public final java.util.Optional<hydra.compute.Context<M>> schema;
  
  public Context (java.util.Map<hydra.core.Name, java.util.Optional<hydra.core.Term<M>>> environment, hydra.core.Term<M> body, hydra.graph.Graph<M> graph, java.util.Map<hydra.core.Name, hydra.compute.Primitive<M>> primitives, hydra.compute.AnnotationClass<M> annotations, java.util.Optional<hydra.compute.Context<M>> schema) {
    this.environment = environment;
    this.body = body;
    this.graph = graph;
    this.primitives = primitives;
    this.annotations = annotations;
    this.schema = schema;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Context)) {
      return false;
    }
    Context o = (Context) (other);
    return environment.equals(o.environment) && body.equals(o.body) && graph.equals(o.graph) && primitives.equals(o.primitives) && annotations.equals(o.annotations) && schema.equals(o.schema);
  }
  
  @Override
  public int hashCode() {
    return 2 * environment.hashCode() + 3 * body.hashCode() + 5 * graph.hashCode() + 7 * primitives.hashCode() + 11 * annotations.hashCode() + 13 * schema.hashCode();
  }
  
  public Context withEnvironment(java.util.Map<hydra.core.Name, java.util.Optional<hydra.core.Term<M>>> environment) {
    return new Context(environment, body, graph, primitives, annotations, schema);
  }
  
  public Context withBody(hydra.core.Term<M> body) {
    return new Context(environment, body, graph, primitives, annotations, schema);
  }
  
  public Context withGraph(hydra.graph.Graph<M> graph) {
    return new Context(environment, body, graph, primitives, annotations, schema);
  }
  
  public Context withPrimitives(java.util.Map<hydra.core.Name, hydra.compute.Primitive<M>> primitives) {
    return new Context(environment, body, graph, primitives, annotations, schema);
  }
  
  public Context withAnnotations(hydra.compute.AnnotationClass<M> annotations) {
    return new Context(environment, body, graph, primitives, annotations, schema);
  }
  
  public Context withSchema(java.util.Optional<hydra.compute.Context<M>> schema) {
    return new Context(environment, body, graph, primitives, annotations, schema);
  }
}