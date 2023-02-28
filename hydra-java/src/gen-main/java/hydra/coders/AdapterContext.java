package hydra.coders;

/**
 * An evaluation context together with a source language and a target language
 */
public class AdapterContext<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/coders.AdapterContext");
  
  public final hydra.graph.Graph<M> graph;
  
  public final hydra.coders.Language<M> source;
  
  public final hydra.coders.Language<M> target;
  
  public AdapterContext (hydra.graph.Graph<M> graph, hydra.coders.Language<M> source, hydra.coders.Language<M> target) {
    this.graph = graph;
    this.source = source;
    this.target = target;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdapterContext)) {
      return false;
    }
    AdapterContext o = (AdapterContext) (other);
    return graph.equals(o.graph) && source.equals(o.source) && target.equals(o.target);
  }
  
  @Override
  public int hashCode() {
    return 2 * graph.hashCode() + 3 * source.hashCode() + 5 * target.hashCode();
  }
  
  public AdapterContext withGraph(hydra.graph.Graph<M> graph) {
    return new AdapterContext(graph, source, target);
  }
  
  public AdapterContext withSource(hydra.coders.Language<M> source) {
    return new AdapterContext(graph, source, target);
  }
  
  public AdapterContext withTarget(hydra.coders.Language<M> target) {
    return new AdapterContext(graph, source, target);
  }
}