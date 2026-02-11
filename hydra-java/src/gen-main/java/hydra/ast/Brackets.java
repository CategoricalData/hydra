// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Matching open and close bracket symbols
 */
public class Brackets implements Serializable, Comparable<Brackets> {
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
    this.open = open;
    this.close = close;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Brackets)) {
      return false;
    }
    Brackets o = (Brackets) other;
    return java.util.Objects.equals(
      this.open,
      o.open) && java.util.Objects.equals(
      this.close,
      o.close);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(open) + 3 * java.util.Objects.hashCode(close);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Brackets other) {
    int cmp = 0;
    cmp = ((Comparable) open).compareTo(other.open);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) close).compareTo(other.close);
  }
  
  public Brackets withOpen(hydra.ast.Symbol open) {
    return new Brackets(open, close);
  }
  
  public Brackets withClose(hydra.ast.Symbol close) {
    return new Brackets(open, close);
  }
}
