// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Source implements Serializable, Comparable<Source> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Source");

  public static final hydra.core.Name STATS = new hydra.core.Name("stats");

  public final java.util.List<hydra.ext.scala.syntax.Stat> stats;

  public Source (java.util.List<hydra.ext.scala.syntax.Stat> stats) {
    this.stats = stats;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Source)) {
      return false;
    }
    Source o = (Source) other;
    return java.util.Objects.equals(
      this.stats,
      o.stats);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(stats);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Source other) {
    return hydra.util.Comparing.compare(
      stats,
      other.stats);
  }
}
