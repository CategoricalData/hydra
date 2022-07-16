package hydra.ext.xml.schema;

public class NormalizedString {
  public final String value;
  
  public NormalizedString (String value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalizedString)) {
      return false;
    }
    NormalizedString o = (NormalizedString) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}