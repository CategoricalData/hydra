// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodHeader implements Serializable, Comparable<MethodHeader> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodHeader");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_RESULT = new hydra.core.Name("result");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATOR = new hydra.core.Name("declarator");
  
  public static final hydra.core.Name FIELD_NAME_THROWS = new hydra.core.Name("throws");
  
  public final java.util.List<hydra.ext.java.syntax.TypeParameter> parameters;
  
  public final hydra.ext.java.syntax.Result result;
  
  public final hydra.ext.java.syntax.MethodDeclarator declarator;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Throws> throws_;
  
  public MethodHeader (java.util.List<hydra.ext.java.syntax.TypeParameter> parameters, hydra.ext.java.syntax.Result result, hydra.ext.java.syntax.MethodDeclarator declarator, hydra.util.Maybe<hydra.ext.java.syntax.Throws> throws_) {
    this.parameters = parameters;
    this.result = result;
    this.declarator = declarator;
    this.throws_ = throws_;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodHeader)) {
      return false;
    }
    MethodHeader o = (MethodHeader) other;
    return java.util.Objects.equals(
      this.parameters,
      o.parameters) && java.util.Objects.equals(
      this.result,
      o.result) && java.util.Objects.equals(
      this.declarator,
      o.declarator) && java.util.Objects.equals(
      this.throws_,
      o.throws_);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parameters) + 3 * java.util.Objects.hashCode(result) + 5 * java.util.Objects.hashCode(declarator) + 7 * java.util.Objects.hashCode(throws_);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodHeader other) {
    int cmp = 0;
    cmp = Integer.compare(
      parameters.hashCode(),
      other.parameters.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) result).compareTo(other.result);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) declarator).compareTo(other.declarator);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      throws_.hashCode(),
      other.throws_.hashCode());
  }
  
  public MethodHeader withParameters(java.util.List<hydra.ext.java.syntax.TypeParameter> parameters) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }
  
  public MethodHeader withResult(hydra.ext.java.syntax.Result result) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }
  
  public MethodHeader withDeclarator(hydra.ext.java.syntax.MethodDeclarator declarator) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }
  
  public MethodHeader withThrows(hydra.util.Maybe<hydra.ext.java.syntax.Throws> throws_) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }
}
