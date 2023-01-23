package hydra.ext.sql.ansi;

public class AttributeOrMethodReference {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.AttributeOrMethodReference");
  
  public AttributeOrMethodReference () {
  
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AttributeOrMethodReference)) {
      return false;
    }
    AttributeOrMethodReference o = (AttributeOrMethodReference) (other);
    return true;
  }
  
  @Override
  public int hashCode() {
    return 0;
  }
}