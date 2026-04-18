// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class AsymmetricObjectProperty implements Serializable, Comparable<AsymmetricObjectProperty> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.AsymmetricObjectProperty");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public final java.util.List<hydra.owl.syntax.Annotation> annotations;

  public final hydra.owl.syntax.ObjectPropertyExpression property;

  public AsymmetricObjectProperty (java.util.List<hydra.owl.syntax.Annotation> annotations, hydra.owl.syntax.ObjectPropertyExpression property) {
    this.annotations = annotations;
    this.property = property;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AsymmetricObjectProperty)) {
      return false;
    }
    AsymmetricObjectProperty o = (AsymmetricObjectProperty) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property,
      o.property);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AsymmetricObjectProperty other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      property,
      other.property);
  }

  public AsymmetricObjectProperty withAnnotations(java.util.List<hydra.owl.syntax.Annotation> annotations) {
    return new AsymmetricObjectProperty(annotations, property);
  }

  public AsymmetricObjectProperty withProperty(hydra.owl.syntax.ObjectPropertyExpression property) {
    return new AsymmetricObjectProperty(annotations, property);
  }
}
