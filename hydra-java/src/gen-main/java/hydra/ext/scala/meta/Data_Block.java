package hydra.ext.scala.meta;

public class Data_Block {
  public final java.util.List<Stat> stats;
  
  public Data_Block (java.util.List<Stat> stats) {
    this.stats = stats;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Block)) {
      return false;
    }
    Data_Block o = (Data_Block) (other);
    return stats.equals(o.stats);
  }
  
  @Override
  public int hashCode() {
    return 2 * stats.hashCode();
  }
}