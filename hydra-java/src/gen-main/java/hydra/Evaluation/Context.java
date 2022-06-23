package hydra.evaluation;

/**
 * A pointed set of graph modules; a graph in the logical sense
 */
public class Context<M> {
  public final hydra.graph.GraphSet<M> graphs;
  
  public final java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements;
  
  public final java.util.Map<hydra.core.Name, PrimitiveFunction<M>> functions;
  
  public final EvaluationStrategy strategy;
  
  public final java.util.function.Function<M, Result<java.util.Optional<String>>> descriptionOf;
  
  public final java.util.function.Function<M, Result<java.util.Optional<hydra.core.Type<M>>>> typeOf;
  
  public final java.util.function.Function<java.util.Optional<String>, java.util.function.Function<M, M>> setDescriptionOf;
  
  public final java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<M, M>> setTypeOf;
  
  public Context (hydra.graph.GraphSet<M> graphs, java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements, java.util.Map<hydra.core.Name, PrimitiveFunction<M>> functions, EvaluationStrategy strategy, java.util.function.Function<M, Result<java.util.Optional<String>>> descriptionOf, java.util.function.Function<M, Result<java.util.Optional<hydra.core.Type<M>>>> typeOf, java.util.function.Function<java.util.Optional<String>, java.util.function.Function<M, M>> setDescriptionOf, java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<M, M>> setTypeOf) {
    this.graphs = graphs;
    this.elements = elements;
    this.functions = functions;
    this.strategy = strategy;
    this.descriptionOf = descriptionOf;
    this.typeOf = typeOf;
    this.setDescriptionOf = setDescriptionOf;
    this.setTypeOf = setTypeOf;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Context)) {
      return false;
    }
    Context o = (Context) (other);
    return graphs.equals(o.graphs) && elements.equals(o.elements) && functions.equals(o.functions) && strategy.equals(o.strategy) && descriptionOf.equals(o.descriptionOf) && typeOf.equals(o.typeOf) && setDescriptionOf.equals(o.setDescriptionOf) && setTypeOf.equals(o.setTypeOf);
  }
  
  @Override
  public int hashCode() {
    return 2 * graphs.hashCode() + 3 * elements.hashCode() + 5 * functions.hashCode() + 7 * strategy.hashCode() + 11 * descriptionOf.hashCode() + 13 * typeOf.hashCode() + 17 * setDescriptionOf.hashCode() + 19 * setTypeOf.hashCode();
  }
  
  public Context withGraphs(hydra.graph.GraphSet<M> graphs) {
    return new Context(graphs, elements, functions, strategy, descriptionOf, typeOf, setDescriptionOf, setTypeOf);
  }
  
  public Context withElements(java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements) {
    return new Context(graphs, elements, functions, strategy, descriptionOf, typeOf, setDescriptionOf, setTypeOf);
  }
  
  public Context withFunctions(java.util.Map<hydra.core.Name, PrimitiveFunction<M>> functions) {
    return new Context(graphs, elements, functions, strategy, descriptionOf, typeOf, setDescriptionOf, setTypeOf);
  }
  
  public Context withStrategy(EvaluationStrategy strategy) {
    return new Context(graphs, elements, functions, strategy, descriptionOf, typeOf, setDescriptionOf, setTypeOf);
  }
  
  public Context withDescriptionOf(java.util.function.Function<M, Result<java.util.Optional<String>>> descriptionOf) {
    return new Context(graphs, elements, functions, strategy, descriptionOf, typeOf, setDescriptionOf, setTypeOf);
  }
  
  public Context withTypeOf(java.util.function.Function<M, Result<java.util.Optional<hydra.core.Type<M>>>> typeOf) {
    return new Context(graphs, elements, functions, strategy, descriptionOf, typeOf, setDescriptionOf, setTypeOf);
  }
  
  public Context withSetDescriptionOf(java.util.function.Function<java.util.Optional<String>, java.util.function.Function<M, M>> setDescriptionOf) {
    return new Context(graphs, elements, functions, strategy, descriptionOf, typeOf, setDescriptionOf, setTypeOf);
  }
  
  public Context withSetTypeOf(java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<M, M>> setTypeOf) {
    return new Context(graphs, elements, functions, strategy, descriptionOf, typeOf, setDescriptionOf, setTypeOf);
  }
}