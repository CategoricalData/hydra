package hydra.ext.shex.syntax;

public class ValueSet {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shex/syntax.ValueSet");
  
  public final java.util.List<hydra.ext.shex.syntax.ValueSetValue> listOfValueSetValue;
  
  public ValueSet (java.util.List<hydra.ext.shex.syntax.ValueSetValue> listOfValueSetValue) {
    this.listOfValueSetValue = listOfValueSetValue;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ValueSet)) {
      return false;
    }
    ValueSet o = (ValueSet) (other);
    return listOfValueSetValue.equals(o.listOfValueSetValue);
  }
  
  @Override
  public int hashCode() {
    return 2 * listOfValueSetValue.hashCode();
  }
}