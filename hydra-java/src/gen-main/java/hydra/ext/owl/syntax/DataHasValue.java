package hydra.ext.owl.syntax;

public class DataHasValue {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.DataHasValue");
  
  public final hydra.ext.owl.syntax.DataPropertyExpression property;
  
  public final hydra.ext.rdf.syntax.Literal value;
  
  public DataHasValue (hydra.ext.owl.syntax.DataPropertyExpression property, hydra.ext.rdf.syntax.Literal value) {
    this.property = property;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataHasValue)) {
      return false;
    }
    DataHasValue o = (DataHasValue) (other);
    return property.equals(o.property) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * property.hashCode() + 3 * value.hashCode();
  }
  
  public DataHasValue withProperty(hydra.ext.owl.syntax.DataPropertyExpression property) {
    return new DataHasValue(property, value);
  }
  
  public DataHasValue withValue(hydra.ext.rdf.syntax.Literal value) {
    return new DataHasValue(property, value);
  }
}