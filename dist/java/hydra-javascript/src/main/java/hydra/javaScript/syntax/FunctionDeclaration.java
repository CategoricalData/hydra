// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A function declaration
 */
public class FunctionDeclaration implements Serializable, Comparable<FunctionDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.FunctionDeclaration");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name ASYNC = new hydra.core.Name("async");

  public static final hydra.core.Name GENERATOR = new hydra.core.Name("generator");

  /**
   * Function name
   */
  public final hydra.javaScript.syntax.Identifier id;

  /**
   * Function parameters
   */
  public final java.util.List<hydra.javaScript.syntax.Pattern> params;

  /**
   * Function body
   */
  public final java.util.List<hydra.javaScript.syntax.Statement> body;

  /**
   * Whether the function is async
   */
  public final Boolean async;

  /**
   * Whether the function is a generator
   */
  public final Boolean generator;

  public FunctionDeclaration (hydra.javaScript.syntax.Identifier id, java.util.List<hydra.javaScript.syntax.Pattern> params, java.util.List<hydra.javaScript.syntax.Statement> body, Boolean async, Boolean generator) {
    this.id = id;
    this.params = params;
    this.body = body;
    this.async = async;
    this.generator = generator;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionDeclaration)) {
      return false;
    }
    FunctionDeclaration o = (FunctionDeclaration) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.params,
      o.params) && java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.async,
      o.async) && java.util.Objects.equals(
      this.generator,
      o.generator);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(params) + 5 * java.util.Objects.hashCode(body) + 7 * java.util.Objects.hashCode(async) + 11 * java.util.Objects.hashCode(generator);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      params,
      other.params);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      async,
      other.async);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      generator,
      other.generator);
  }

  public FunctionDeclaration withId(hydra.javaScript.syntax.Identifier id) {
    return new FunctionDeclaration(id, params, body, async, generator);
  }

  public FunctionDeclaration withParams(java.util.List<hydra.javaScript.syntax.Pattern> params) {
    return new FunctionDeclaration(id, params, body, async, generator);
  }

  public FunctionDeclaration withBody(java.util.List<hydra.javaScript.syntax.Statement> body) {
    return new FunctionDeclaration(id, params, body, async, generator);
  }

  public FunctionDeclaration withAsync(Boolean async) {
    return new FunctionDeclaration(id, params, body, async, generator);
  }

  public FunctionDeclaration withGenerator(Boolean generator) {
    return new FunctionDeclaration(id, params, body, async, generator);
  }
}
