// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class MethodDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.MethodDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_RECEIVER_PARAMETER = new hydra.core.Name("receiverParameter");
  
  public static final hydra.core.Name FIELD_NAME_FORMAL_PARAMETERS = new hydra.core.Name("formalParameters");
  
  public final hydra.langs.java.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.ReceiverParameter> receiverParameter;
  
  public final java.util.List<hydra.langs.java.syntax.FormalParameter> formalParameters;
  
  public MethodDeclarator (hydra.langs.java.syntax.Identifier identifier, hydra.util.Opt<hydra.langs.java.syntax.ReceiverParameter> receiverParameter, java.util.List<hydra.langs.java.syntax.FormalParameter> formalParameters) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((receiverParameter));
    java.util.Objects.requireNonNull((formalParameters));
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
    java.util.Objects.requireNonNull((identifier));
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withReceiverParameter(hydra.util.Opt<hydra.langs.java.syntax.ReceiverParameter> receiverParameter) {
    java.util.Objects.requireNonNull((receiverParameter));
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withFormalParameters(java.util.List<hydra.langs.java.syntax.FormalParameter> formalParameters) {
    java.util.Objects.requireNonNull((formalParameters));
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
}