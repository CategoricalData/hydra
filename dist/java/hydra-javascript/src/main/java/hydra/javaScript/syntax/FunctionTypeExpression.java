// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A function type expression
 */
public class FunctionTypeExpression implements Serializable, Comparable<FunctionTypeExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.FunctionTypeExpression");

  public static final hydra.core.Name TYPE_PARAMETERS = new hydra.core.Name("typeParameters");

  public static final hydra.core.Name PARAMETERS = new hydra.core.Name("parameters");

  public static final hydra.core.Name RETURN_TYPE = new hydra.core.Name("returnType");

  /**
   * Type parameters (generics)
   */
  public final java.util.List<hydra.javaScript.syntax.TypeParameter> typeParameters;

  /**
   * Parameter types
   */
  public final java.util.List<hydra.javaScript.syntax.TypeExpression> parameters;

  /**
   * Return type
   */
  public final hydra.javaScript.syntax.TypeExpression returnType;

  public FunctionTypeExpression (java.util.List<hydra.javaScript.syntax.TypeParameter> typeParameters, java.util.List<hydra.javaScript.syntax.TypeExpression> parameters, hydra.javaScript.syntax.TypeExpression returnType) {
    this.typeParameters = typeParameters;
    this.parameters = parameters;
    this.returnType = returnType;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionTypeExpression)) {
      return false;
    }
    FunctionTypeExpression o = (FunctionTypeExpression) other;
    return java.util.Objects.equals(
      this.typeParameters,
      o.typeParameters) && java.util.Objects.equals(
      this.parameters,
      o.parameters) && java.util.Objects.equals(
      this.returnType,
      o.returnType);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeParameters) + 3 * java.util.Objects.hashCode(parameters) + 5 * java.util.Objects.hashCode(returnType);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionTypeExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typeParameters,
      other.typeParameters);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      parameters,
      other.parameters);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      returnType,
      other.returnType);
  }

  public FunctionTypeExpression withTypeParameters(java.util.List<hydra.javaScript.syntax.TypeParameter> typeParameters) {
    return new FunctionTypeExpression(typeParameters, parameters, returnType);
  }

  public FunctionTypeExpression withParameters(java.util.List<hydra.javaScript.syntax.TypeExpression> parameters) {
    return new FunctionTypeExpression(typeParameters, parameters, returnType);
  }

  public FunctionTypeExpression withReturnType(hydra.javaScript.syntax.TypeExpression returnType) {
    return new FunctionTypeExpression(typeParameters, parameters, returnType);
  }
}
