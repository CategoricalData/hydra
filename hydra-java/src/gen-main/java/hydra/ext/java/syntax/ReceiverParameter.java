// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ReceiverParameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ReceiverParameter");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_UNANN_TYPE = new hydra.core.Name("unannType");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final java.util.List<hydra.ext.java.syntax.Annotation> annotations;
  
  public final hydra.ext.java.syntax.UnannType unannType;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.Identifier> identifier;
  
  public ReceiverParameter (java.util.List<hydra.ext.java.syntax.Annotation> annotations, hydra.ext.java.syntax.UnannType unannType, hydra.util.Opt<hydra.ext.java.syntax.Identifier> identifier) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((unannType));
    java.util.Objects.requireNonNull((identifier));
    this.annotations = annotations;
    this.unannType = unannType;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReceiverParameter)) {
      return false;
    }
    ReceiverParameter o = (ReceiverParameter) (other);
    return annotations.equals(o.annotations) && unannType.equals(o.unannType) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * unannType.hashCode() + 5 * identifier.hashCode();
  }
  
  public ReceiverParameter withAnnotations(java.util.List<hydra.ext.java.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new ReceiverParameter(annotations, unannType, identifier);
  }
  
  public ReceiverParameter withUnannType(hydra.ext.java.syntax.UnannType unannType) {
    java.util.Objects.requireNonNull((unannType));
    return new ReceiverParameter(annotations, unannType, identifier);
  }
  
  public ReceiverParameter withIdentifier(hydra.util.Opt<hydra.ext.java.syntax.Identifier> identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new ReceiverParameter(annotations, unannType, identifier);
  }
}