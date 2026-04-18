// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class PopStringsArgument implements Serializable, Comparable<PopStringsArgument> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.PopStringsArgument");

  public static final hydra.core.Name POP = new hydra.core.Name("pop");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public final hydra.tinkerpop.gremlin.TraversalPopArgument pop;

  public final java.util.List<hydra.tinkerpop.gremlin.StringArgument> string;

  public PopStringsArgument (hydra.tinkerpop.gremlin.TraversalPopArgument pop, java.util.List<hydra.tinkerpop.gremlin.StringArgument> string) {
    this.pop = pop;
    this.string = string;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PopStringsArgument)) {
      return false;
    }
    PopStringsArgument o = (PopStringsArgument) other;
    return java.util.Objects.equals(
      this.pop,
      o.pop) && java.util.Objects.equals(
      this.string,
      o.string);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(pop) + 3 * java.util.Objects.hashCode(string);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PopStringsArgument other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      pop,
      other.pop);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      string,
      other.string);
  }

  public PopStringsArgument withPop(hydra.tinkerpop.gremlin.TraversalPopArgument pop) {
    return new PopStringsArgument(pop, string);
  }

  public PopStringsArgument withString(java.util.List<hydra.tinkerpop.gremlin.StringArgument> string) {
    return new PopStringsArgument(pop, string);
  }
}
