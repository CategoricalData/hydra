// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A type parameter (generic)
 */
public class TypeParameter implements Serializable, Comparable<TypeParameter> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.TypeParameter");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name CONSTRAINT = new hydra.core.Name("constraint");

  public static final hydra.core.Name DEFAULT = new hydra.core.Name("default");

  /**
   * Parameter name
   */
  public final hydra.javaScript.syntax.Identifier name;

  /**
   * Optional constraint (extends clause)
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.TypeExpression> constraint;

  /**
   * Optional default type
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.TypeExpression> default_;

  public TypeParameter (hydra.javaScript.syntax.Identifier name, hydra.util.Maybe<hydra.javaScript.syntax.TypeExpression> constraint, hydra.util.Maybe<hydra.javaScript.syntax.TypeExpression> default_) {
    this.name = name;
    this.constraint = constraint;
    this.default_ = default_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeParameter)) {
      return false;
    }
    TypeParameter o = (TypeParameter) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.constraint,
      o.constraint) && java.util.Objects.equals(
      this.default_,
      o.default_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(constraint) + 5 * java.util.Objects.hashCode(default_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TypeParameter other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      constraint,
      other.constraint);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      default_,
      other.default_);
  }

  public TypeParameter withName(hydra.javaScript.syntax.Identifier name) {
    return new TypeParameter(name, constraint, default_);
  }

  public TypeParameter withConstraint(hydra.util.Maybe<hydra.javaScript.syntax.TypeExpression> constraint) {
    return new TypeParameter(name, constraint, default_);
  }

  public TypeParameter withDefault(hydra.util.Maybe<hydra.javaScript.syntax.TypeExpression> default_) {
    return new TypeParameter(name, constraint, default_);
  }
}
