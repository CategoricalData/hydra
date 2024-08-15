// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class DisjointObjectProperties implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/owl/syntax.DisjointObjectProperties");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> properties;
  
  public DisjointObjectProperties (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> properties) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((properties));
    this.annotations = annotations;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DisjointObjectProperties)) {
      return false;
    }
    DisjointObjectProperties o = (DisjointObjectProperties) (other);
    return annotations.equals(o.annotations) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * properties.hashCode();
  }
  
  public DisjointObjectProperties withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new DisjointObjectProperties(annotations, properties);
  }
  
  public DisjointObjectProperties withProperties(java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> properties) {
    java.util.Objects.requireNonNull((properties));
    return new DisjointObjectProperties(annotations, properties);
  }
}