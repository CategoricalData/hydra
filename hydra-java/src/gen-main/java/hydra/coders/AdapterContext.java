package hydra.coders;

/**
 * An evaluation context together with a source language and a target language
 */
public class AdapterContext<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/coders.AdapterContext");
  
  public final hydra.graph.Graph<A> graph;
  
  public final hydra.coders.Language<A> source;
  
  public final hydra.coders.Language<A> target;
  
  public final java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext<A>, hydra.coders.AdapterContext<A>, hydra.core.Type<A>, hydra.core.Type<A>, hydra.core.Term<A>, hydra.core.Term<A>>> adapters;
  
  public AdapterContext (hydra.graph.Graph<A> graph, hydra.coders.Language<A> source, hydra.coders.Language<A> target, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext<A>, hydra.coders.AdapterContext<A>, hydra.core.Type<A>, hydra.core.Type<A>, hydra.core.Term<A>, hydra.core.Term<A>>> adapters) {
    this.graph = graph;
    this.source = source;
    this.target = target;
    this.adapters = adapters;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdapterContext)) {
      return false;
    }
    AdapterContext o = (AdapterContext) (other);
    return graph.equals(o.graph) && source.equals(o.source) && target.equals(o.target) && adapters.equals(o.adapters);
  }
  
  @Override
  public int hashCode() {
    return 2 * graph.hashCode() + 3 * source.hashCode() + 5 * target.hashCode() + 7 * adapters.hashCode();
  }
  
  public AdapterContext withGraph(hydra.graph.Graph<A> graph) {
    return new AdapterContext(graph, source, target, adapters);
  }
  
  public AdapterContext withSource(hydra.coders.Language<A> source) {
    return new AdapterContext(graph, source, target, adapters);
  }
  
  public AdapterContext withTarget(hydra.coders.Language<A> target) {
    return new AdapterContext(graph, source, target, adapters);
  }
  
  public AdapterContext withAdapters(java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext<A>, hydra.coders.AdapterContext<A>, hydra.core.Type<A>, hydra.core.Type<A>, hydra.core.Term<A>, hydra.core.Term<A>>> adapters) {
    return new AdapterContext(graph, source, target, adapters);
  }
}