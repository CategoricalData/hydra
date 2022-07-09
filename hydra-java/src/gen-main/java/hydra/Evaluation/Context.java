package hydra.evaluation;

/**
 * A pointed set of graph modules; a graph in the logical sense
 */
public class Context<M> {
  public final hydra.graph.GraphSet<M> graphs;
  
  public final java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements;
  
  public final java.util.Map<hydra.core.Name, PrimitiveFunction<M>> functions;
  
  public final EvaluationStrategy strategy;
  
  public final java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<String>>> description_OfTerm;
  
  public final java.util.function.Function<hydra.core.Type<M>, Result<java.util.Optional<String>>> description_OfType;
  
  public final java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<hydra.core.Type<M>>>> type_OfTerm;
  
  public final java.util.function.Function<java.util.Optional<String>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>> setDescription_OfTerm;
  
  public final java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>> setType_OfTerm;
  
  public final java.util.function.Function<M, Result<java.util.Optional<hydra.core.Type<M>>>> typeOf;
  
  public final java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<M, M>> setTypeOf;
  
  public Context (hydra.graph.GraphSet<M> graphs, java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements, java.util.Map<hydra.core.Name, PrimitiveFunction<M>> functions, EvaluationStrategy strategy, java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<String>>> description_OfTerm, java.util.function.Function<hydra.core.Type<M>, Result<java.util.Optional<String>>> description_OfType, java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<hydra.core.Type<M>>>> type_OfTerm, java.util.function.Function<java.util.Optional<String>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>> setDescription_OfTerm, java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>> setType_OfTerm, java.util.function.Function<M, Result<java.util.Optional<hydra.core.Type<M>>>> typeOf, java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<M, M>> setTypeOf) {
    this.graphs = graphs;
    this.elements = elements;
    this.functions = functions;
    this.strategy = strategy;
    this.description_OfTerm = description_OfTerm;
    this.description_OfType = description_OfType;
    this.type_OfTerm = type_OfTerm;
    this.setDescription_OfTerm = setDescription_OfTerm;
    this.setType_OfTerm = setType_OfTerm;
    this.typeOf = typeOf;
    this.setTypeOf = setTypeOf;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Context)) {
      return false;
    }
    Context o = (Context) (other);
    return graphs.equals(o.graphs) && elements.equals(o.elements) && functions.equals(o.functions) && strategy.equals(o.strategy) && description_OfTerm.equals(o.description_OfTerm) && description_OfType.equals(o.description_OfType) && type_OfTerm.equals(o.type_OfTerm) && setDescription_OfTerm.equals(o.setDescription_OfTerm) && setType_OfTerm.equals(o.setType_OfTerm) && typeOf.equals(o.typeOf) && setTypeOf.equals(o.setTypeOf);
  }
  
  @Override
  public int hashCode() {
    return 2 * graphs.hashCode() + 3 * elements.hashCode() + 5 * functions.hashCode() + 7 * strategy.hashCode() + 11 * description_OfTerm.hashCode() + 13 * description_OfType.hashCode() + 17 * type_OfTerm.hashCode() + 19 * setDescription_OfTerm.hashCode() + 23 * setType_OfTerm.hashCode() + 29 * typeOf.hashCode() + 31 * setTypeOf.hashCode();
  }
  
  public Context withGraphs(hydra.graph.GraphSet<M> graphs) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withElements(java.util.Map<hydra.core.Name, hydra.graph.Element<M>> elements) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withFunctions(java.util.Map<hydra.core.Name, PrimitiveFunction<M>> functions) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withStrategy(EvaluationStrategy strategy) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withDescription_OfTerm(java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<String>>> description_OfTerm) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withDescription_OfType(java.util.function.Function<hydra.core.Type<M>, Result<java.util.Optional<String>>> description_OfType) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withType_OfTerm(java.util.function.Function<hydra.core.Term<M>, Result<java.util.Optional<hydra.core.Type<M>>>> type_OfTerm) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withSetDescription_OfTerm(java.util.function.Function<java.util.Optional<String>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>> setDescription_OfTerm) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withSetType_OfTerm(java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<hydra.core.Term<M>, hydra.core.Term<M>>> setType_OfTerm) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withTypeOf(java.util.function.Function<M, Result<java.util.Optional<hydra.core.Type<M>>>> typeOf) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
  
  public Context withSetTypeOf(java.util.function.Function<java.util.Optional<hydra.core.Type<M>>, java.util.function.Function<M, M>> setTypeOf) {
    return new Context(graphs, elements, functions, strategy, description_OfTerm, description_OfType, type_OfTerm, setDescription_OfTerm, setType_OfTerm, typeOf, setTypeOf);
  }
}