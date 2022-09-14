package hydra.evaluation;

/**
 * A pointed set of graph modules; a graph in the logical sense
 */
public class Context<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/evaluation.Context");
  
  public final hydra.graph.GraphSet<M> graphs;
  
  public final java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements;
  
  public final java.util.Map<hydra.core.Name, hydra.evaluation.PrimitiveFunction<M>> functions;
  
  public final hydra.evaluation.EvaluationStrategy strategy;
  
  public final hydra.evaluation.AnnotationClass<M> annotations;
  
  public Context (hydra.graph.GraphSet<M> graphs, java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements, java.util.Map<hydra.core.Name, hydra.evaluation.PrimitiveFunction<M>> functions, hydra.evaluation.EvaluationStrategy strategy, hydra.evaluation.AnnotationClass<M> annotations) {
    this.graphs = graphs;
    this.elements = elements;
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
    return graphs.equals(o.graphs) && elements.equals(o.elements) && functions.equals(o.functions) && strategy.equals(o.strategy) && annotations.equals(o.annotations);
  }
  
  @Override
  public int hashCode() {
    return 2 * graphs.hashCode() + 3 * elements.hashCode() + 5 * functions.hashCode() + 7 * strategy.hashCode() + 11 * annotations.hashCode();
  }
  
  public Context withGraphs(hydra.graph.GraphSet<M> graphs) {
    return new Context(graphs, elements, functions, strategy, annotations);
  }
  
  public Context withElements(java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements) {
    return new Context(graphs, elements, functions, strategy, annotations);
  }
  
  public Context withFunctions(java.util.Map<hydra.core.Name, hydra.evaluation.PrimitiveFunction<M>> functions) {
    return new Context(graphs, elements, functions, strategy, annotations);
  }
  
  public Context withStrategy(hydra.evaluation.EvaluationStrategy strategy) {
    return new Context(graphs, elements, functions, strategy, annotations);
  }
  
  public Context withAnnotations(hydra.evaluation.AnnotationClass<M> annotations) {
    return new Context(graphs, elements, functions, strategy, annotations);
  }
}