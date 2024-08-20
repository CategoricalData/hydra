// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ConstructorDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.ConstructorDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_RECEIVER_PARAMETER = new hydra.core.Name("receiverParameter");
  
  public static final hydra.core.Name FIELD_NAME_FORMAL_PARAMETERS = new hydra.core.Name("formalParameters");
  
  public final java.util.List<hydra.ext.java.syntax.TypeParameter> parameters;
  
  public final hydra.ext.java.syntax.SimpleTypeName name;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.ReceiverParameter> receiverParameter;
  
  public final java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters;
  
  public ConstructorDeclarator (java.util.List<hydra.ext.java.syntax.TypeParameter> parameters, hydra.ext.java.syntax.SimpleTypeName name, hydra.util.Opt<hydra.ext.java.syntax.ReceiverParameter> receiverParameter, java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
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
  
  public ConstructorDeclarator withParameters(java.util.List<hydra.ext.java.syntax.TypeParameter> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withName(hydra.ext.java.syntax.SimpleTypeName name) {
    java.util.Objects.requireNonNull((name));
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withReceiverParameter(hydra.util.Opt<hydra.ext.java.syntax.ReceiverParameter> receiverParameter) {
    java.util.Objects.requireNonNull((receiverParameter));
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withFormalParameters(java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
    java.util.Objects.requireNonNull((formalParameters));
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
}
