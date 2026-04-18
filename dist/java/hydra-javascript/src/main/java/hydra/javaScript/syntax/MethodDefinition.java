// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A method definition in a class
 */
public class MethodDefinition implements Serializable, Comparable<MethodDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.MethodDefinition");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name KIND = new hydra.core.Name("kind");

  public static final hydra.core.Name COMPUTED = new hydra.core.Name("computed");

  public static final hydra.core.Name STATIC = new hydra.core.Name("static");

  /**
   * Method name
   */
  public final hydra.javaScript.syntax.Expression key;

  /**
   * Method function
   */
  public final hydra.javaScript.syntax.FunctionExpression value;

  /**
   * Method kind
   */
  public final hydra.javaScript.syntax.MethodKind kind;

  /**
   * Whether the key is computed
   */
  public final Boolean computed;

  /**
   * Whether the method is static
   */
  public final Boolean static_;

  public MethodDefinition (hydra.javaScript.syntax.Expression key, hydra.javaScript.syntax.FunctionExpression value, hydra.javaScript.syntax.MethodKind kind, Boolean computed, Boolean static_) {
    this.key = key;
    this.value = value;
    this.kind = kind;
    this.computed = computed;
    this.static_ = static_;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodDefinition)) {
      return false;
    }
    MethodDefinition o = (MethodDefinition) other;
    return java.util.Objects.equals(
      this.key,
      o.key) && java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.kind,
      o.kind) && java.util.Objects.equals(
      this.computed,
      o.computed) && java.util.Objects.equals(
      this.static_,
      o.static_);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(key) + 3 * java.util.Objects.hashCode(value) + 5 * java.util.Objects.hashCode(kind) + 7 * java.util.Objects.hashCode(computed) + 11 * java.util.Objects.hashCode(static_);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MethodDefinition other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      key,
      other.key);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      kind,
      other.kind);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      computed,
      other.computed);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      static_,
      other.static_);
  }

  public MethodDefinition withKey(hydra.javaScript.syntax.Expression key) {
    return new MethodDefinition(key, value, kind, computed, static_);
  }

  public MethodDefinition withValue(hydra.javaScript.syntax.FunctionExpression value) {
    return new MethodDefinition(key, value, kind, computed, static_);
  }

  public MethodDefinition withKind(hydra.javaScript.syntax.MethodKind kind) {
    return new MethodDefinition(key, value, kind, computed, static_);
  }

  public MethodDefinition withComputed(Boolean computed) {
    return new MethodDefinition(key, value, kind, computed, static_);
  }

  public MethodDefinition withStatic(Boolean static_) {
    return new MethodDefinition(key, value, kind, computed, static_);
  }
}
