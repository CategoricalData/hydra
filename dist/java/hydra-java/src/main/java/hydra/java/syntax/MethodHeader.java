// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class MethodHeader implements Serializable, Comparable<MethodHeader> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.MethodHeader");

  public static final hydra.core.Name PARAMETERS = new hydra.core.Name("parameters");

  public static final hydra.core.Name RESULT = new hydra.core.Name("result");

  public static final hydra.core.Name DECLARATOR = new hydra.core.Name("declarator");

  public static final hydra.core.Name THROWS = new hydra.core.Name("throws");

  public final java.util.List<hydra.java.syntax.TypeParameter> parameters;

  public final hydra.java.syntax.Result result;

  public final hydra.java.syntax.MethodDeclarator declarator;

  public final hydra.util.Maybe<hydra.java.syntax.Throws> throws_;

  public MethodHeader (java.util.List<hydra.java.syntax.TypeParameter> parameters, hydra.java.syntax.Result result, hydra.java.syntax.MethodDeclarator declarator, hydra.util.Maybe<hydra.java.syntax.Throws> throws_) {
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
    cmp = hydra.util.Comparing.compare(
      parameters,
      other.parameters);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      result,
      other.result);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      declarator,
      other.declarator);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      throws_,
      other.throws_);
  }

  public MethodHeader withParameters(java.util.List<hydra.java.syntax.TypeParameter> parameters) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }

  public MethodHeader withResult(hydra.java.syntax.Result result) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }

  public MethodHeader withDeclarator(hydra.java.syntax.MethodDeclarator declarator) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }

  public MethodHeader withThrows(hydra.util.Maybe<hydra.java.syntax.Throws> throws_) {
    return new MethodHeader(parameters, result, declarator, throws_);
  }
}
