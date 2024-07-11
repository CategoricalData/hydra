// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class MethodDeclarator implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.MethodDeclarator");
  
  public final hydra.langs.java.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.ReceiverParameter> receiverParameter;
  
  public final java.util.List<hydra.langs.java.syntax.FormalParameter> formalParameters;
  
  public MethodDeclarator (hydra.langs.java.syntax.Identifier identifier, hydra.util.Opt<hydra.langs.java.syntax.ReceiverParameter> receiverParameter, java.util.List<hydra.langs.java.syntax.FormalParameter> formalParameters) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    if (receiverParameter == null) {
      throw new IllegalArgumentException("null value for 'receiverParameter' argument");
    }
    if (formalParameters == null) {
      throw new IllegalArgumentException("null value for 'formalParameters' argument");
    }
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
  
  public MethodDeclarator withIdentifier(hydra.langs.java.syntax.Identifier identifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withReceiverParameter(hydra.util.Opt<hydra.langs.java.syntax.ReceiverParameter> receiverParameter) {
    if (receiverParameter == null) {
      throw new IllegalArgumentException("null value for 'receiverParameter' argument");
    }
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withFormalParameters(java.util.List<hydra.langs.java.syntax.FormalParameter> formalParameters) {
    if (formalParameters == null) {
      throw new IllegalArgumentException("null value for 'formalParameters' argument");
    }
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
}