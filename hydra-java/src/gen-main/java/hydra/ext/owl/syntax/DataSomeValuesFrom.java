package hydra.ext.owl.syntax;

public class DataSomeValuesFrom {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.DataSomeValuesFrom");
  
  public final java.util.List<hydra.ext.owl.syntax.DataPropertyExpression> property;
  
  public final hydra.ext.owl.syntax.DataRange range;
  
  public DataSomeValuesFrom (java.util.List<hydra.ext.owl.syntax.DataPropertyExpression> property, hydra.ext.owl.syntax.DataRange range) {
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataSomeValuesFrom)) {
      return false;
    }
    DataSomeValuesFrom o = (DataSomeValuesFrom) (other);
    return property.equals(o.property) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * property.hashCode() + 3 * range.hashCode();
  }
  
  public DataSomeValuesFrom withProperty(java.util.List<hydra.ext.owl.syntax.DataPropertyExpression> property) {
    return new DataSomeValuesFrom(property, range);
  }
  
  public DataSomeValuesFrom withRange(hydra.ext.owl.syntax.DataRange range) {
    return new DataSomeValuesFrom(property, range);
  }
}