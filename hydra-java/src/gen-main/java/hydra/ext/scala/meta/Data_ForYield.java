package hydra.ext.scala.meta;

public class Data_ForYield {
  public final java.util.List<Enumerator> enums;
  
  public Data_ForYield (java.util.List<Enumerator> enums) {
    this.enums = enums;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_ForYield)) {
      return false;
    }
    Data_ForYield o = (Data_ForYield) (other);
    return enums.equals(o.enums);
  }
  
  @Override
  public int hashCode() {
    return 2 * enums.hashCode();
  }
}