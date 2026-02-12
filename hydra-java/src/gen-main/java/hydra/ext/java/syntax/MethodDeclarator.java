// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodDeclarator implements Serializable, Comparable<MethodDeclarator> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_RECEIVER_PARAMETER = new hydra.core.Name("receiverParameter");
  
  public static final hydra.core.Name FIELD_NAME_FORMAL_PARAMETERS = new hydra.core.Name("formalParameters");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter> receiverParameter;
  
  public final java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters;
  
  public MethodDeclarator (hydra.ext.java.syntax.Identifier identifier, hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter> receiverParameter, java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
    this.identifier = identifier;
    this.receiverParameter = receiverParameter;
    this.formalParameters = formalParameters;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodDeclarator)) {
      return false;
    }
    MethodDeclarator o = (MethodDeclarator) other;
    return java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.receiverParameter,
      o.receiverParameter) && java.util.Objects.equals(
      this.formalParameters,
      o.formalParameters);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(identifier) + 3 * java.util.Objects.hashCode(receiverParameter) + 5 * java.util.Objects.hashCode(formalParameters);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodDeclarator other) {
    int cmp = 0;
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      receiverParameter.hashCode(),
      other.receiverParameter.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      formalParameters.hashCode(),
      other.formalParameters.hashCode());
  }
  
  public MethodDeclarator withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withReceiverParameter(hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter> receiverParameter) {
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
  
  public MethodDeclarator withFormalParameters(java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
    return new MethodDeclarator(identifier, receiverParameter, formalParameters);
  }
}
