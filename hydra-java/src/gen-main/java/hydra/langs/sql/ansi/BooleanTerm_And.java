// Note: this is an automatically generated file. Do not edit.

package hydra.langs.sql.ansi;

import java.io.Serializable;

public class BooleanTerm_And implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/sql/ansi.BooleanTerm.And");
  
  public final hydra.langs.sql.ansi.BooleanTerm lhs;
  
  public final hydra.langs.sql.ansi.BooleanFactor rhs;
  
  public BooleanTerm_And (hydra.langs.sql.ansi.BooleanTerm lhs, hydra.langs.sql.ansi.BooleanFactor rhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BooleanTerm_And)) {
      return false;
    }
    BooleanTerm_And o = (BooleanTerm_And) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public BooleanTerm_And withLhs(hydra.langs.sql.ansi.BooleanTerm lhs) {
    if (lhs == null) {
      throw new IllegalArgumentException("null value for 'lhs' argument");
    }
    return new BooleanTerm_And(lhs, rhs);
  }
  
  public BooleanTerm_And withRhs(hydra.langs.sql.ansi.BooleanFactor rhs) {
    if (rhs == null) {
      throw new IllegalArgumentException("null value for 'rhs' argument");
    }
    return new BooleanTerm_And(lhs, rhs);
  }
}