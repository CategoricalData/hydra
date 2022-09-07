package hydra.ext.java.syntax;

public class MethodDeclarator {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.MethodDeclarator");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final java.util.Optional<hydra.ext.java.syntax.ReceiverParameter> receiverParameter;
  
  public final java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters;
  
  public MethodDeclarator (hydra.ext.java.syntax.Identifier identifier, java.util.Optional<hydra.ext.java.syntax.ReceiverParameter> receiverParameter, java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
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
  
  public MethodDeclarator withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withReceiverParameter(java.util.Optional<hydra.ext.java.syntax.ReceiverParameter> receiverParameter) {
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withFormalParameters(java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
}