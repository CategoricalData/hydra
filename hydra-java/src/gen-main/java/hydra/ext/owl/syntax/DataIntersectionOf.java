package hydra.ext.owl.syntax;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges
 */
public class DataIntersectionOf {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.DataIntersectionOf");
  
  /**
   * See https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges
   */
  public final java.util.List<hydra.ext.owl.syntax.DataRange> value;
  
  public DataIntersectionOf (java.util.List<hydra.ext.owl.syntax.DataRange> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataIntersectionOf)) {
      return false;
    }
    DataIntersectionOf o = (DataIntersectionOf) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}