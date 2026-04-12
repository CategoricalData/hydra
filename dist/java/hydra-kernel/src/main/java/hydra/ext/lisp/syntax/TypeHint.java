// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A type hint or annotation. In Clojure: ^Type name. In Common Lisp: (declare (type Type name)). In Scheme and Emacs Lisp: typically unused.
 */
public class TypeHint implements Serializable, Comparable<TypeHint> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.TypeHint");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  /**
   * The annotated symbol
   */
  public final hydra.ext.lisp.syntax.Symbol name;

  /**
   * The type specifier
   */
  public final hydra.ext.lisp.syntax.TypeSpecifier type;

  public TypeHint (hydra.ext.lisp.syntax.Symbol name, hydra.ext.lisp.syntax.TypeSpecifier type) {
    this.name = name;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeHint)) {
      return false;
    }
    TypeHint o = (TypeHint) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeHint other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public TypeHint withName(hydra.ext.lisp.syntax.Symbol name) {
    return new TypeHint(name, type);
  }

  public TypeHint withType(hydra.ext.lisp.syntax.TypeSpecifier type) {
    return new TypeHint(name, type);
  }
}
