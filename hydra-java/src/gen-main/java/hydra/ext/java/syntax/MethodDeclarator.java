package hydra.ext.java.syntax;

public class MethodDeclarator {
  public final Identifier identifier;
  
  public final java.util.Optional<ReceiverParameter> receiverParameter;
  
  /**
   * Note: list cannot be empty
   */
  public final java.util.List<FormalParameter> formalParameters;
  
  public MethodDeclarator (Identifier identifier, java.util.Optional<ReceiverParameter> receiverParameter, java.util.List<FormalParameter> formalParameters) {
    this.identifier = identifier;
    this.receiverParameter = receiverParameter;
    this.formalParameters = formalParameters;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodDeclarator)) {
      return false;
    }
    MethodDeclarator o = (MethodDeclarator) (other);
    return identifier.equals(o.identifier) && receiverParameter.equals(o.receiverParameter) && formalParameters.equals(o.formalParameters);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * receiverParameter.hashCode() + 5 * formalParameters.hashCode();
  }
  
  public MethodDeclarator withIdentifier(Identifier identifier) {
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withReceiverParameter(java.util.Optional<ReceiverParameter> receiverParameter) {
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withFormalParameters(java.util.List<FormalParameter> formalParameters) {
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
}