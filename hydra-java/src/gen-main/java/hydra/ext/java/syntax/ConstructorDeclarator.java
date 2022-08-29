package hydra.ext.java.syntax;

public class ConstructorDeclarator {
  public final java.util.List<hydra.ext.java.syntax.TypeParameter> parameters;
  
  public final hydra.ext.java.syntax.SimpleTypeName name;
  
  public final java.util.Optional<hydra.ext.java.syntax.ReceiverParameter> receiverParameter;
  
  public final java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters;
  
  public ConstructorDeclarator (java.util.List<hydra.ext.java.syntax.TypeParameter> parameters, hydra.ext.java.syntax.SimpleTypeName name, java.util.Optional<hydra.ext.java.syntax.ReceiverParameter> receiverParameter, java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
    this.parameters = parameters;
    this.name = name;
    this.receiverParameter = receiverParameter;
    this.formalParameters = formalParameters;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorDeclarator)) {
      return false;
    }
    ConstructorDeclarator o = (ConstructorDeclarator) (other);
    return parameters.equals(o.parameters) && name.equals(o.name) && receiverParameter.equals(o.receiverParameter) && formalParameters.equals(o.formalParameters);
  }
  
  @Override
  public int hashCode() {
    return 2 * parameters.hashCode() + 3 * name.hashCode() + 5 * receiverParameter.hashCode() + 7 * formalParameters.hashCode();
  }
  
  public ConstructorDeclarator withParameters(java.util.List<hydra.ext.java.syntax.TypeParameter> parameters) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withName(hydra.ext.java.syntax.SimpleTypeName name) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withReceiverParameter(java.util.Optional<hydra.ext.java.syntax.ReceiverParameter> receiverParameter) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withFormalParameters(java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
}