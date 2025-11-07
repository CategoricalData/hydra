// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Matching open and close bracket symbols
 */
public class Brackets implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.Brackets");
  
  public static final hydra.core.Name FIELD_NAME_OPEN = new hydra.core.Name("open");
  
  public static final hydra.core.Name FIELD_NAME_CLOSE = new hydra.core.Name("close");
  
  /**
   * The opening bracket symbol
   */
  public final hydra.ast.Symbol open;
  
  /**
   * The closing bracket symbol
   */
  public final hydra.ast.Symbol close;
  
  public Brackets (hydra.ast.Symbol open, hydra.ast.Symbol close) {
    java.util.Objects.requireNonNull((open));
    java.util.Objects.requireNonNull((close));
    this.open = open;
    this.close = close;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Brackets)) {
      return false;
    }
    Brackets o = (Brackets) (other);
    return open.equals(o.open) && close.equals(o.close);
  }
  
  @Override
  public int hashCode() {
    return 2 * open.hashCode() + 3 * close.hashCode();
  }
  
  public Brackets withOpen(hydra.ast.Symbol open) {
    java.util.Objects.requireNonNull((open));
    return new Brackets(open, close);
  }
  
  public Brackets withClose(hydra.ast.Symbol close) {
    java.util.Objects.requireNonNull((close));
    return new Brackets(open, close);
  }
}
