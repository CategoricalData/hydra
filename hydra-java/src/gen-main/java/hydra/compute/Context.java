package hydra.compute;

/**
 * An environment containing a graph together with primitive functions and other necessary components for evaluation
 */
public class Context<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.Context");
  
  /**
   * The graph itself
   */
  public final hydra.mantle.Graph<M> graph;
  
  /**
   * All supported primitive functions, by name
   */
  public final java.util.Map<hydra.core.Name, hydra.compute.Primitive<M>> functions;
  
  /**
   * The annotation class which is supported in this context
   */
  public final hydra.compute.AnnotationClass<M> annotations;
  
  public Context (hydra.mantle.Graph<M> graph, java.util.Map<hydra.core.Name, hydra.compute.Primitive<M>> functions, hydra.compute.AnnotationClass<M> annotations) {
    this.graph = graph;
    this.functions = functions;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Context)) {
      return false;
    }
    Context o = (Context) (other);
    return graph.equals(o.graph) && functions.equals(o.functions) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * graph.hashCode() + 3 * functions.hashCode() + 5 * annotations.hashCode();
  }
  
  public Context withGraph(hydra.mantle.Graph<M> graph) {
    return new Context(graph, functions, annotations);
  }
  
  public Context withFunctions(java.util.Map<hydra.core.Name, hydra.compute.Primitive<M>> functions) {
    return new Context(graph, functions, annotations);
  }
  
  public Context withAnnotations(hydra.compute.AnnotationClass<M> annotations) {
    return new Context(graph, functions, annotations);
  }
}