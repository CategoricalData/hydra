// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class ConstructorDeclarator implements Serializable, Comparable<ConstructorDeclarator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ConstructorDeclarator");

  public static final hydra.core.Name PARAMETERS = new hydra.core.Name("parameters");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name RECEIVER_PARAMETER = new hydra.core.Name("receiverParameter");

  public static final hydra.core.Name FORMAL_PARAMETERS = new hydra.core.Name("formalParameters");

  public final java.util.List<hydra.java.syntax.TypeParameter> parameters;

  public final hydra.java.syntax.SimpleTypeName name;

  public final hydra.util.Maybe<hydra.java.syntax.ReceiverParameter> receiverParameter;

  public final java.util.List<hydra.java.syntax.FormalParameter> formalParameters;

  public ConstructorDeclarator (java.util.List<hydra.java.syntax.TypeParameter> parameters, hydra.java.syntax.SimpleTypeName name, hydra.util.Maybe<hydra.java.syntax.ReceiverParameter> receiverParameter, java.util.List<hydra.java.syntax.FormalParameter> formalParameters) {
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
    cmp = hydra.util.Comparing.compare(
      parameters,
      other.parameters);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      receiverParameter,
      other.receiverParameter);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      formalParameters,
      other.formalParameters);
  }

  public ConstructorDeclarator withParameters(java.util.List<hydra.java.syntax.TypeParameter> parameters) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }

  public ConstructorDeclarator withName(hydra.java.syntax.SimpleTypeName name) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }

  public ConstructorDeclarator withReceiverParameter(hydra.util.Maybe<hydra.java.syntax.ReceiverParameter> receiverParameter) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }

  public ConstructorDeclarator withFormalParameters(java.util.List<hydra.java.syntax.FormalParameter> formalParameters) {
    return new ConstructorDeclarator(parameters, name, receiverParameter, formalParameters);
  }
}
