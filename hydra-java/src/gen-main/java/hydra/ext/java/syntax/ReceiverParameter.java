package hydra.ext.java.syntax;

public class ReceiverParameter {
  public final java.util.List<Annotation> annotations;
  
  public final UnannType unannType;
  
  public final java.util.Optional<Identifier> identifier;
  
  public ReceiverParameter (java.util.List<Annotation> annotations, UnannType unannType, java.util.Optional<Identifier> identifier) {
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
  
  public ReceiverParameter withAnnotations(java.util.List<Annotation> annotations) {
    return new ReceiverParameter(annotations, unannType, identifier);
  }
  
  public ReceiverParameter withUnannType(UnannType unannType) {
    return new ReceiverParameter(annotations, unannType, identifier);
  }
  
  public ReceiverParameter withIdentifier(java.util.Optional<Identifier> identifier) {
    return new ReceiverParameter(annotations, unannType, identifier);
  }
}