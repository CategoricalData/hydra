// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Block implements Serializable, Comparable<Data_Block> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Data_Block");

  public static final hydra.core.Name STATS = new hydra.core.Name("stats");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Stat> stats;

  public Data_Block (hydra.util.ConsList<hydra.ext.scala.meta.Stat> stats) {
    this.stats = stats;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Block)) {
      return false;
    }
    Data_Block o = (Data_Block) other;
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
  public int compareTo(Data_Block other) {
    return ((Comparable) stats).compareTo(other.stats);
  }
}
