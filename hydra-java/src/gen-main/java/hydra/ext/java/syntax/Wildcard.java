// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class Wildcard implements Serializable, Comparable<Wildcard> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.Wildcard");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_WILDCARD = new hydra.core.Name("wildcard");
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.WildcardBounds> wildcard;
  
  public Wildcard (java.util.List<hydra.ext.java.syntax.Annotation> annotations, hydra.util.Maybe<hydra.ext.java.syntax.WildcardBounds> wildcard) {
    this.annotations = annotations;
    this.wildcard = wildcard;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Wildcard)) {
      return false;
    }
    Wildcard o = (Wildcard) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.wildcard,
      o.wildcard);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(wildcard);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Wildcard other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      wildcard.hashCode(),
      other.wildcard.hashCode());
  }
  
  public Wildcard withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    return new Wildcard(annotations, wildcard);
  }
  
  public Wildcard withWildcard(hydra.util.Maybe<hydra.ext.java.syntax.WildcardBounds> wildcard) {
    return new Wildcard(annotations, wildcard);
  }
}
