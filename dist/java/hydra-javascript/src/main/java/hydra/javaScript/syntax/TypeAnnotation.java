// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A type annotation (for JSDoc comments or TypeScript)
 */
public class TypeAnnotation implements Serializable, Comparable<TypeAnnotation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.TypeAnnotation");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.javaScript.syntax.TypeExpression value;

  public TypeAnnotation (hydra.javaScript.syntax.TypeExpression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeAnnotation)) {
      return false;
    }
    TypeAnnotation o = (TypeAnnotation) other;
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
  public int compareTo(TypeAnnotation other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
