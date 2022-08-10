package hydra.ext.java.syntax;

public class ConstructorDeclarator {
  public final java.util.List<TypeParameter> parameters;
  
  public final SimpleTypeName name;
  
  public final java.util.Optional<ReceiverParameter> receiverParameter;
  
  public final java.util.List<FormalParameter> formalParameters;
  
  public ConstructorDeclarator (java.util.List<TypeParameter> parameters, SimpleTypeName name, java.util.Optional<ReceiverParameter> receiverParameter, java.util.List<FormalParameter> formalParameters) {
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
  
  public ConstructorDeclarator withParameters(java.util.List<TypeParameter> parameters) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withName(SimpleTypeName name) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withReceiverParameter(java.util.Optional<ReceiverParameter> receiverParameter) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withFormalParameters(java.util.List<FormalParameter> formalParameters) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
}