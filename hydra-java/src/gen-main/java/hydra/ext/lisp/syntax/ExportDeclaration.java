// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * An export/provide declaration. Serializes as (provide 'name) in Emacs Lisp, (:export :sym1 :sym2) in Common Lisp, (export sym1 sym2) in Scheme. In Clojure, symbols are public by default.
 */
public class ExportDeclaration implements Serializable, Comparable<ExportDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.ExportDeclaration");

  public static final hydra.core.Name SYMBOLS = new hydra.core.Name("symbols");

  /**
   * The symbols to export
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> symbols;

  public ExportDeclaration (hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> symbols) {
    this.symbols = symbols;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExportDeclaration)) {
      return false;
    }
    ExportDeclaration o = (ExportDeclaration) other;
    return java.util.Objects.equals(
      this.symbols,
      o.symbols);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(symbols);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ExportDeclaration other) {
    return ((Comparable) symbols).compareTo(other.symbols);
  }
}
