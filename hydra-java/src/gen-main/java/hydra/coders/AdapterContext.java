// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

import java.io.Serializable;

/**
 * An evaluation context together with a source language and a target language
 */
public class AdapterContext implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.coders.AdapterContext");
  
  public static final hydra.core.Name FIELD_NAME_GRAPH = new hydra.core.Name("graph");
  
  public static final hydra.core.Name FIELD_NAME_LANGUAGE = new hydra.core.Name("language");
  
  public static final hydra.core.Name FIELD_NAME_ADAPTERS = new hydra.core.Name("adapters");
  
  /**
   * The underlying graph of elements and primitives
   */
  public final hydra.graph.Graph graph;
  
  /**
   * The language being encoded or decoded
   */
  public final hydra.coders.Language language;
  
  /**
   * A map of type names to adapters for those types
   */
  public final java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> adapters;
  
  public AdapterContext (hydra.graph.Graph graph, hydra.coders.Language language, java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> adapters) {
    java.util.Objects.requireNonNull((graph));
    java.util.Objects.requireNonNull((language));
    java.util.Objects.requireNonNull((adapters));
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
  
  public AdapterContext withGraph(hydra.graph.Graph graph) {
    java.util.Objects.requireNonNull((graph));
    return new AdapterContext(graph, language, adapters);
  }
  
  public AdapterContext withLanguage(hydra.coders.Language language) {
    java.util.Objects.requireNonNull((language));
    return new AdapterContext(graph, language, adapters);
  }
  
  public AdapterContext withAdapters(java.util.Map<hydra.core.Name, hydra.compute.Adapter<hydra.coders.AdapterContext, hydra.coders.AdapterContext, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>> adapters) {
    java.util.Objects.requireNonNull((adapters));
    return new AdapterContext(graph, language, adapters);
  }
}
