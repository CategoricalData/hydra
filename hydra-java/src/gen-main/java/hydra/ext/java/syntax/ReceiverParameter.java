// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ReceiverParameter implements Serializable, Comparable<ReceiverParameter> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ReceiverParameter");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_UNANN_TYPE = new hydra.core.Name("unannType");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final hydra.ext.java.syntax.UnannType unannType;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Identifier> identifier;
  
  public ReceiverParameter (java.util.List<hydra.ext.java.syntax.Annotation> annotations, hydra.ext.java.syntax.UnannType unannType, hydra.util.Maybe<hydra.ext.java.syntax.Identifier> identifier) {
    this.annotations = annotations;
    this.unannType = unannType;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReceiverParameter)) {
      return false;
    }
    ReceiverParameter o = (ReceiverParameter) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.unannType,
      o.unannType) && java.util.Objects.equals(
      this.identifier,
      o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(unannType) + 5 * java.util.Objects.hashCode(identifier);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ReceiverParameter other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) unannType).compareTo(other.unannType);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      identifier.hashCode(),
      other.identifier.hashCode());
  }
  
  public ReceiverParameter withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    return new ReceiverParameter(annotations, unannType, identifier);
  }
  
  public ReceiverParameter withUnannType(hydra.ext.java.syntax.UnannType unannType) {
    return new ReceiverParameter(annotations, unannType, identifier);
  }
  
  public ReceiverParameter withIdentifier(hydra.util.Maybe<hydra.ext.java.syntax.Identifier> identifier) {
    return new ReceiverParameter(annotations, unannType, identifier);
  }
}
