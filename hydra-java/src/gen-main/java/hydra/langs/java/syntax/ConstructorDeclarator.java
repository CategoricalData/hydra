// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class ConstructorDeclarator implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.ConstructorDeclarator");
  
  public final java.util.List<hydra.langs.java.syntax.TypeParameter> parameters;
  
  public final hydra.langs.java.syntax.SimpleTypeName name;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.ReceiverParameter> receiverParameter;
  
  public final java.util.List<hydra.langs.java.syntax.FormalParameter> formalParameters;
  
  public ConstructorDeclarator (java.util.List<hydra.langs.java.syntax.TypeParameter> parameters, hydra.langs.java.syntax.SimpleTypeName name, hydra.util.Opt<hydra.langs.java.syntax.ReceiverParameter> receiverParameter, java.util.List<hydra.langs.java.syntax.FormalParameter> formalParameters) {
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((receiverParameter));
    java.util.Objects.requireNonNull((formalParameters));
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
  
  public ConstructorDeclarator withParameters(java.util.List<hydra.langs.java.syntax.TypeParameter> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withName(hydra.langs.java.syntax.SimpleTypeName name) {
    java.util.Objects.requireNonNull((name));
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withReceiverParameter(hydra.util.Opt<hydra.langs.java.syntax.ReceiverParameter> receiverParameter) {
    java.util.Objects.requireNonNull((receiverParameter));
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withFormalParameters(java.util.List<hydra.langs.java.syntax.FormalParameter> formalParameters) {
    java.util.Objects.requireNonNull((formalParameters));
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
}