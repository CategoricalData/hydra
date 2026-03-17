// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A test case which performs topological sort on a map of bindings (name -&gt; term) and compares the result with expected groups of bindings in topological order
 */
public class TopologicalSortBindingsTestCase implements Serializable, Comparable<TopologicalSortBindingsTestCase> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase");

  public static final hydra.core.Name BINDINGS = new hydra.core.Name("bindings");

  public static final hydra.core.Name EXPECTED = new hydra.core.Name("expected");

  /**
   * The bindings as a list of (name, term) pairs
   */
  public final hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>> bindings;

  /**
   * The expected groups of bindings in topological order
   */
  public final hydra.util.ConsList<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> expected;

  public TopologicalSortBindingsTestCase (hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>> bindings, hydra.util.ConsList<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> expected) {
    this.bindings = bindings;
    this.expected = expected;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TopologicalSortBindingsTestCase)) {
      return false;
    }
    TopologicalSortBindingsTestCase o = (TopologicalSortBindingsTestCase) other;
    return java.util.Objects.equals(
      this.bindings,
      o.bindings) && java.util.Objects.equals(
      this.expected,
      o.expected);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(bindings) + 3 * java.util.Objects.hashCode(expected);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TopologicalSortBindingsTestCase other) {
    int cmp = 0;
    cmp = ((Comparable) bindings).compareTo(other.bindings);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) expected).compareTo(other.expected);
  }

  public TopologicalSortBindingsTestCase withBindings(hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>> bindings) {
    return new TopologicalSortBindingsTestCase(bindings, expected);
  }

  public TopologicalSortBindingsTestCase withExpected(hydra.util.ConsList<hydra.util.ConsList<hydra.util.Pair<hydra.core.Name, hydra.core.Term>>> expected) {
    return new TopologicalSortBindingsTestCase(bindings, expected);
  }
}
