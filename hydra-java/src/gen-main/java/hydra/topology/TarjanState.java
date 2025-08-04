// Note: this is an automatically generated file. Do not edit.

package hydra.topology;

import java.io.Serializable;

public class TarjanState implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.topology.TarjanState");
  
  public static final hydra.core.Name FIELD_NAME_COUNTER = new hydra.core.Name("counter");
  
  public static final hydra.core.Name FIELD_NAME_INDICES = new hydra.core.Name("indices");
  
  public static final hydra.core.Name FIELD_NAME_LOW_LINKS = new hydra.core.Name("lowLinks");
  
  public static final hydra.core.Name FIELD_NAME_STACK = new hydra.core.Name("stack");
  
  public static final hydra.core.Name FIELD_NAME_ON_STACK = new hydra.core.Name("onStack");
  
  public static final hydra.core.Name FIELD_NAME_SCCS = new hydra.core.Name("sccs");
  
  /**
   * Next available index for vertices in the DFS traversal
   */
  public final Integer counter;
  
  /**
   * Mapping from vertices to their indices in the DFS traversal
   */
  public final java.util.Map<hydra.topology.Vertex, Integer> indices;
  
  /**
   * Mapping from vertices to their lowest reachable index in the DFS traversal
   */
  public final java.util.Map<hydra.topology.Vertex, Integer> lowLinks;
  
  /**
   * Current DFS stack, with vertices in reverse order
   */
  public final java.util.List<hydra.topology.Vertex> stack;
  
  /**
   * Set of vertices currently on the stack, for quick lookup
   */
  public final java.util.Set<hydra.topology.Vertex> onStack;
  
  /**
   * Accumulated strongly connected components, each a list of vertices
   */
  public final java.util.List<java.util.List<hydra.topology.Vertex>> sccs;
  
  public TarjanState (Integer counter, java.util.Map<hydra.topology.Vertex, Integer> indices, java.util.Map<hydra.topology.Vertex, Integer> lowLinks, java.util.List<hydra.topology.Vertex> stack, java.util.Set<hydra.topology.Vertex> onStack, java.util.List<java.util.List<hydra.topology.Vertex>> sccs) {
    java.util.Objects.requireNonNull((counter));
    java.util.Objects.requireNonNull((indices));
    java.util.Objects.requireNonNull((lowLinks));
    java.util.Objects.requireNonNull((stack));
    java.util.Objects.requireNonNull((onStack));
    java.util.Objects.requireNonNull((sccs));
    this.counter = counter;
    this.indices = indices;
    this.lowLinks = lowLinks;
    this.stack = stack;
    this.onStack = onStack;
    this.sccs = sccs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TarjanState)) {
      return false;
    }
    TarjanState o = (TarjanState) (other);
    return counter.equals(o.counter) && indices.equals(o.indices) && lowLinks.equals(o.lowLinks) && stack.equals(o.stack) && onStack.equals(o.onStack) && sccs.equals(o.sccs);
  }
  
  @Override
  public int hashCode() {
    return 2 * counter.hashCode() + 3 * indices.hashCode() + 5 * lowLinks.hashCode() + 7 * stack.hashCode() + 11 * onStack.hashCode() + 13 * sccs.hashCode();
  }
  
  public TarjanState withCounter(Integer counter) {
    java.util.Objects.requireNonNull((counter));
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }
  
  public TarjanState withIndices(java.util.Map<hydra.topology.Vertex, Integer> indices) {
    java.util.Objects.requireNonNull((indices));
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }
  
  public TarjanState withLowLinks(java.util.Map<hydra.topology.Vertex, Integer> lowLinks) {
    java.util.Objects.requireNonNull((lowLinks));
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }
  
  public TarjanState withStack(java.util.List<hydra.topology.Vertex> stack) {
    java.util.Objects.requireNonNull((stack));
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }
  
  public TarjanState withOnStack(java.util.Set<hydra.topology.Vertex> onStack) {
    java.util.Objects.requireNonNull((onStack));
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }
  
  public TarjanState withSccs(java.util.List<java.util.List<hydra.topology.Vertex>> sccs) {
    java.util.Objects.requireNonNull((sccs));
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }
}
