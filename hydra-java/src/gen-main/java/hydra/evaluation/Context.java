package hydra.evaluation;

/**
 * An environment containing a graph together with primitive functions and other necessary components for evaluation
 */
public class Context<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/evaluation.Context");
  
  public final hydra.graph.Graph<M> graph;
  
  public final java.util.Map<hydra.core.Name, hydra.evaluation.PrimitiveFunction<M>> functions;
  
  public final hydra.evaluation.EvaluationStrategy strategy;
  
  public final hydra.evaluation.AnnotationClass<M> annotations;
  
  public Context (hydra.graph.Graph<M> graph, java.util.Map<hydra.core.Name, hydra.evaluation.PrimitiveFunction<M>> functions, hydra.evaluation.EvaluationStrategy strategy, hydra.evaluation.AnnotationClass<M> annotations) {
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
  
  public Context withGraph(hydra.graph.Graph<M> graph) {
    return new Context(graph, functions, strategy, annotations);
  }
  
  public Context withFunctions(java.util.Map<hydra.core.Name, hydra.evaluation.PrimitiveFunction<M>> functions) {
    return new Context(graph, functions, strategy, annotations);
  }
  
  public Context withStrategy(hydra.evaluation.EvaluationStrategy strategy) {
    return new Context(graph, functions, strategy, annotations);
  }
  
  public Context withAnnotations(hydra.evaluation.AnnotationClass<M> annotations) {
    return new Context(graph, functions, strategy, annotations);
  }
}