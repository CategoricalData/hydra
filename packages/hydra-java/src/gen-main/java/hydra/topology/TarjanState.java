// Note: this is an automatically generated file. Do not edit.

package hydra.topology;

import java.io.Serializable;

public class TarjanState implements Serializable, Comparable<TarjanState> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.topology.TarjanState");

  public static final hydra.core.Name COUNTER = new hydra.core.Name("counter");

  public static final hydra.core.Name INDICES = new hydra.core.Name("indices");

  public static final hydra.core.Name LOW_LINKS = new hydra.core.Name("lowLinks");

  public static final hydra.core.Name STACK = new hydra.core.Name("stack");

  public static final hydra.core.Name ON_STACK = new hydra.core.Name("onStack");

  public static final hydra.core.Name SCCS = new hydra.core.Name("sccs");

  /**
   * Next available index for vertices in the DFS traversal
   */
  public final Integer counter;

  /**
   * Mapping from vertices to their indices in the DFS traversal
   */
  public final java.util.Map<Integer, Integer> indices;

  /**
   * Mapping from vertices to their lowest reachable index in the DFS traversal
   */
  public final java.util.Map<Integer, Integer> lowLinks;

  /**
   * Current DFS stack, with vertices in reverse order
   */
  public final java.util.List<Integer> stack;

  /**
   * Set of vertices currently on the stack, for quick lookup
   */
  public final java.util.Set<Integer> onStack;

  /**
   * Accumulated strongly connected components, each a list of vertices
   */
  public final java.util.List<java.util.List<Integer>> sccs;

  public TarjanState (Integer counter, java.util.Map<Integer, Integer> indices, java.util.Map<Integer, Integer> lowLinks, java.util.List<Integer> stack, java.util.Set<Integer> onStack, java.util.List<java.util.List<Integer>> sccs) {
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
    TarjanState o = (TarjanState) other;
    return java.util.Objects.equals(
      this.counter,
      o.counter) && java.util.Objects.equals(
      this.indices,
      o.indices) && java.util.Objects.equals(
      this.lowLinks,
      o.lowLinks) && java.util.Objects.equals(
      this.stack,
      o.stack) && java.util.Objects.equals(
      this.onStack,
      o.onStack) && java.util.Objects.equals(
      this.sccs,
      o.sccs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(counter) + 3 * java.util.Objects.hashCode(indices) + 5 * java.util.Objects.hashCode(lowLinks) + 7 * java.util.Objects.hashCode(stack) + 11 * java.util.Objects.hashCode(onStack) + 13 * java.util.Objects.hashCode(sccs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TarjanState other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      counter,
      other.counter);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      indices,
      other.indices);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      lowLinks,
      other.lowLinks);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      stack,
      other.stack);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      onStack,
      other.onStack);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      sccs,
      other.sccs);
  }

  public TarjanState withCounter(Integer counter) {
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }

  public TarjanState withIndices(java.util.Map<Integer, Integer> indices) {
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }

  public TarjanState withLowLinks(java.util.Map<Integer, Integer> lowLinks) {
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }

  public TarjanState withStack(java.util.List<Integer> stack) {
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }

  public TarjanState withOnStack(java.util.Set<Integer> onStack) {
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }

  public TarjanState withSccs(java.util.List<java.util.List<Integer>> sccs) {
    return new TarjanState(counter, indices, lowLinks, stack, onStack, sccs);
  }
}
