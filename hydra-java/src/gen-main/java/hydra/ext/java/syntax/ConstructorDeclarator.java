// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class ConstructorDeclarator implements Serializable, Comparable<ConstructorDeclarator> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ConstructorDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_RECEIVER_PARAMETER = new hydra.core.Name("receiverParameter");
  
  public static final hydra.core.Name FIELD_NAME_FORMAL_PARAMETERS = new hydra.core.Name("formalParameters");
  
  public final java.util.List<hydra.ext.java.syntax.TypeParameter> parameters;
  
  public final hydra.ext.java.syntax.SimpleTypeName name;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter> receiverParameter;
  
  public final java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters;
  
  public ConstructorDeclarator (java.util.List<hydra.ext.java.syntax.TypeParameter> parameters, hydra.ext.java.syntax.SimpleTypeName name, hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter> receiverParameter, java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
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
    ConstructorDeclarator o = (ConstructorDeclarator) other;
    return java.util.Objects.equals(
      this.parameters,
      o.parameters) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.receiverParameter,
      o.receiverParameter) && java.util.Objects.equals(
      this.formalParameters,
      o.formalParameters);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parameters) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(receiverParameter) + 7 * java.util.Objects.hashCode(formalParameters);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConstructorDeclarator other) {
    int cmp = 0;
    cmp = Integer.compare(
      parameters.hashCode(),
      other.parameters.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) name).compareTo(other.name);
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
  
  public ConstructorDeclarator withParameters(java.util.List<hydra.ext.java.syntax.TypeParameter> parameters) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withName(hydra.ext.java.syntax.SimpleTypeName name) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withReceiverParameter(hydra.util.Maybe<hydra.ext.java.syntax.ReceiverParameter> receiverParameter) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
  
  public ConstructorDeclarator withFormalParameters(java.util.List<hydra.ext.java.syntax.FormalParameter> formalParameters) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
}
