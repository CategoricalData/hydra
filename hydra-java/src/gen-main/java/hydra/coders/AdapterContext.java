// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

/**
 * An evaluation context together with a source language and a target language
 */
public class AdapterContext<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/coders.AdapterContext");
  
  public final hydra.graph.Graph<A> graph;
  
  public final hydra.coders.Language<A> language;
  
  public final java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext<A>, hydra.coders.AdapterContext<A>, hydra.core.Type<A>, hydra.core.Type<A>, hydra.core.Term<A>, hydra.core.Term<A>>> adapters;
  
  public AdapterContext (hydra.graph.Graph<A> graph, hydra.coders.Language<A> language, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext<A>, hydra.coders.AdapterContext<A>, hydra.core.Type<A>, hydra.core.Type<A>, hydra.core.Term<A>, hydra.core.Term<A>>> adapters) {
    if (graph == null) {
      throw new IllegalArgumentException("null value for 'graph' argument");
    }
    if (language == null) {
      throw new IllegalArgumentException("null value for 'language' argument");
    }
    if (adapters == null) {
      throw new IllegalArgumentException("null value for 'adapters' argument");
    }
    this.graph = graph;
    this.language = language;
    this.adapters = adapters;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AdapterContext)) {
      return false;
    }
    AdapterContext o = (AdapterContext) (other);
    return graph.equals(o.graph) && language.equals(o.language) && adapters.equals(o.adapters);
  }
  
  @Override
  public int hashCode() {
    return 2 * graph.hashCode() + 3 * language.hashCode() + 5 * adapters.hashCode();
  }
  
  public AdapterContext withGraph(hydra.graph.Graph<A> graph) {
    if (graph == null) {
      throw new IllegalArgumentException("null value for 'graph' argument");
    }
    return new AdapterContext(graph, language, adapters);
  }
  
  public AdapterContext withLanguage(hydra.coders.Language<A> language) {
    if (language == null) {
      throw new IllegalArgumentException("null value for 'language' argument");
    }
    return new AdapterContext(graph, language, adapters);
  }
  
  public AdapterContext withAdapters(java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext<A>, hydra.coders.AdapterContext<A>, hydra.core.Type<A>, hydra.core.Type<A>, hydra.core.Term<A>, hydra.core.Term<A>>> adapters) {
    if (adapters == null) {
      throw new IllegalArgumentException("null value for 'adapters' argument");
    }
    return new AdapterContext(graph, language, adapters);
  }
}