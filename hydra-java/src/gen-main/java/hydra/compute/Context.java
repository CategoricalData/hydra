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
  public final java.util.Map<hydra.core.Name, hydra.compute.PrimitiveFunction<M>> functions;
  
  /**
   * The evaluation strategy which is to be used in this context
   */
  public final hydra.compute.EvaluationStrategy strategy;
  
  /**
   * The annotation class which is supported in this context
   */
  public final hydra.compute.AnnotationClass<M> annotations;
  
  public Context (hydra.mantle.Graph<M> graph, java.util.Map<hydra.core.Name, hydra.compute.PrimitiveFunction<M>> functions, hydra.compute.EvaluationStrategy strategy, hydra.compute.AnnotationClass<M> annotations) {
    this.graph = graph;
    this.functions = functions;
    this.strategy = strategy;
    this.annotations = annotations;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Context)) {
      return false;
    }
    Context o = (Context) (other);
    return graph.equals(o.graph) && functions.equals(o.functions) && strategy.equals(o.strategy) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * graph.hashCode() + 3 * functions.hashCode() + 5 * strategy.hashCode() + 7 * annotations.hashCode();
  }
  
  public Context withGraph(hydra.mantle.Graph<M> graph) {
    return new Context(graph, functions, strategy, annotations);
  }
  
  public Context withFunctions(java.util.Map<hydra.core.Name, hydra.compute.PrimitiveFunction<M>> functions) {
    return new Context(graph, functions, strategy, annotations);
  }
  
  public Context withStrategy(hydra.compute.EvaluationStrategy strategy) {
    return new Context(graph, functions, strategy, annotations);
  }
  
  public Context withAnnotations(hydra.compute.AnnotationClass<M> annotations) {
    return new Context(graph, functions, strategy, annotations);
  }
}