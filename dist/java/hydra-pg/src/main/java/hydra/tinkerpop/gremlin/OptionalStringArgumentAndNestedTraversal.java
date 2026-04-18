// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class OptionalStringArgumentAndNestedTraversal implements Serializable, Comparable<OptionalStringArgumentAndNestedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  public final hydra.util.Maybe<hydra.tinkerpop.gremlin.StringArgument> string;

  public final hydra.tinkerpop.gremlin.NestedTraversal traversal;

  public OptionalStringArgumentAndNestedTraversal (hydra.util.Maybe<hydra.tinkerpop.gremlin.StringArgument> string, hydra.tinkerpop.gremlin.NestedTraversal traversal) {
    this.string = string;
    this.traversal = traversal;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof OptionalStringArgumentAndNestedTraversal)) {
      return false;
    }
    OptionalStringArgumentAndNestedTraversal o = (OptionalStringArgumentAndNestedTraversal) other;
    return java.util.Objects.equals(
      this.string,
      o.string) && java.util.Objects.equals(
      this.traversal,
      o.traversal);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(string) + 3 * java.util.Objects.hashCode(traversal);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(OptionalStringArgumentAndNestedTraversal other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      string,
      other.string);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      traversal,
      other.traversal);
  }

  public OptionalStringArgumentAndNestedTraversal withString(hydra.util.Maybe<hydra.tinkerpop.gremlin.StringArgument> string) {
    return new OptionalStringArgumentAndNestedTraversal(string, traversal);
  }

  public OptionalStringArgumentAndNestedTraversal withTraversal(hydra.tinkerpop.gremlin.NestedTraversal traversal) {
    return new OptionalStringArgumentAndNestedTraversal(string, traversal);
  }
}
