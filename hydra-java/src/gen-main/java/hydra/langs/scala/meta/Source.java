// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Source implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Source");
  
  public static final hydra.core.Name FIELD_NAME_STATS = new hydra.core.Name("stats");
  
  public final java.util.List<hydra.langs.scala.meta.Stat> stats;
  
  public Source (java.util.List<hydra.langs.scala.meta.Stat> stats) {
    java.util.Objects.requireNonNull((stats));
    this.stats = stats;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Source)) {
      return false;
    }
    Source o = (Source) (other);
    return stats.equals(o.stats);
  }
  
  @Override
  public int hashCode() {
    return 2 * stats.hashCode();
  }
}