// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Operator precedence
 */
public class Precedence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ast.Precedence");
  
  /**
   * Operator precedence
   */
  public final Integer value;
  
  public Precedence (Integer value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Precedence)) {
      return false;
    }
    Precedence o = (Precedence) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}