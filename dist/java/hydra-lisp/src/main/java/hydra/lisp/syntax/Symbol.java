// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A Lisp symbol (identifier)
 */
public class Symbol implements Serializable, Comparable<Symbol> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.Symbol");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public Symbol (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Symbol)) {
      return false;
    }
    Symbol o = (Symbol) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Symbol other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
