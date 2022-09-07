package hydra.ext.java.syntax;

/**
 * Note: this is an approximation which ignores encoding
 */
public class FloatingPointLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.FloatingPointLiteral");
  
  public final Double value;
  
  public FloatingPointLiteral (Double value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FloatingPointLiteral)) {
      return false;
    }
    FloatingPointLiteral o = (FloatingPointLiteral) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}