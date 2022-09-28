package hydra.ext.owl.syntax;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges
 */
public class DataComplementOf {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.DataComplementOf");
  
  /**
   * See https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges
   */
  public final hydra.ext.owl.syntax.DataRange value;
  
  public DataComplementOf (hydra.ext.owl.syntax.DataRange value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataComplementOf)) {
      return false;
    }
    DataComplementOf o = (DataComplementOf) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}