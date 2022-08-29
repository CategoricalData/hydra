package hydra.ext.scala.meta;

public class Source {
  public final java.util.List<hydra.ext.scala.meta.Stat> stats;
  
  public Source (java.util.List<hydra.ext.scala.meta.Stat> stats) {
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