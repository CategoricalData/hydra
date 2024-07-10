// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * Matching open and close bracket symbols
 */
public class Brackets implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ast.Brackets");
  
  public final hydra.ast.Symbol open;
  
  public final hydra.ast.Symbol close;
  
  public Brackets (hydra.ast.Symbol open, hydra.ast.Symbol close) {
    if (open == null) {
      throw new IllegalArgumentException("null value for 'open' argument");
    }
    if (close == null) {
      throw new IllegalArgumentException("null value for 'close' argument");
    }
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
    if (open == null) {
      throw new IllegalArgumentException("null value for 'open' argument");
    }
    return new Brackets(open, close);
  }
  
  public Brackets withClose(hydra.ast.Symbol close) {
    if (close == null) {
      throw new IllegalArgumentException("null value for 'close' argument");
    }
    return new Brackets(open, close);
  }
}