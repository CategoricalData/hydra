package hydra.util.codetree.ast;

/**
 * Operator precedence
 */
public class Precedence {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/util/codetree/ast.Precedence");
  
  /**
   * Operator precedence
   */
  public final Integer value;
  
  public Precedence (Integer value) {
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