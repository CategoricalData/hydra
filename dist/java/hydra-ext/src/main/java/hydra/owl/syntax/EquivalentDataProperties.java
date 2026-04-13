// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class EquivalentDataProperties implements Serializable, Comparable<EquivalentDataProperties> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.EquivalentDataProperties");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  public final java.util.List<hydra.owl.syntax.Annotation> annotations;

  public final java.util.List<hydra.owl.syntax.DataPropertyExpression> properties;

  public EquivalentDataProperties (java.util.List<hydra.owl.syntax.Annotation> annotations, java.util.List<hydra.owl.syntax.DataPropertyExpression> properties) {
    this.annotations = annotations;
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EquivalentDataProperties)) {
      return false;
    }
    EquivalentDataProperties o = (EquivalentDataProperties) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.properties,
      o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(properties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EquivalentDataProperties other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      properties,
      other.properties);
  }

  public EquivalentDataProperties withAnnotations(java.util.List<hydra.owl.syntax.Annotation> annotations) {
    return new EquivalentDataProperties(annotations, properties);
  }

  public EquivalentDataProperties withProperties(java.util.List<hydra.owl.syntax.DataPropertyExpression> properties) {
    return new EquivalentDataProperties(annotations, properties);
  }
}
