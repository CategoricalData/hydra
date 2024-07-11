// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ReceiverParameter implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ReceiverParameter");
  
  public final java.util.List<hydra.langs.java.syntax.Annotation> annotations;
  
  public final hydra.langs.java.syntax.UnannType unannType;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.Identifier> identifier;
  
  public ReceiverParameter (java.util.List<hydra.langs.java.syntax.Annotation> annotations, hydra.langs.java.syntax.UnannType unannType, hydra.util.Opt<hydra.langs.java.syntax.Identifier> identifier) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (unannType == null) {
      throw new IllegalArgumentException("null value for 'unannType' argument");
    }
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
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
  
  public ReceiverParameter withAnnotations(java.util.List<hydra.langs.java.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new ReceiverParameter(annotations, unannType, identifier);
  }
  
  public ReceiverParameter withUnannType(hydra.langs.java.syntax.UnannType unannType) {
    if (unannType == null) {
      throw new IllegalArgumentException("null value for 'unannType' argument");
    }
    return new ReceiverParameter(annotations, unannType, identifier);
  }
  
  public ReceiverParameter withIdentifier(hydra.util.Opt<hydra.langs.java.syntax.Identifier> identifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    return new ReceiverParameter(annotations, unannType, identifier);
  }
}