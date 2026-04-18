// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A property in an object expression
 */
public class Property implements Serializable, Comparable<Property> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.Property");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name KIND = new hydra.core.Name("kind");

  public static final hydra.core.Name COMPUTED = new hydra.core.Name("computed");

  public static final hydra.core.Name SHORTHAND = new hydra.core.Name("shorthand");

  /**
   * Property key (identifier, literal, or computed)
   */
  public final hydra.javaScript.syntax.Expression key;

  /**
   * Property value
   */
  public final hydra.javaScript.syntax.Expression value;

  /**
   * Property kind (init, get, set)
   */
  public final hydra.javaScript.syntax.PropertyKind kind;

  /**
   * Whether the key is computed [expr]
   */
  public final Boolean computed;

  /**
   * Whether using shorthand syntax {x} for {x: x}
   */
  public final Boolean shorthand;

  public Property (hydra.javaScript.syntax.Expression key, hydra.javaScript.syntax.Expression value, hydra.javaScript.syntax.PropertyKind kind, Boolean computed, Boolean shorthand) {
    this.key = key;
    this.value = value;
    this.kind = kind;
    this.computed = computed;
    this.shorthand = shorthand;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Property)) {
      return false;
    }
    Property o = (Property) other;
    return java.util.Objects.equals(
      this.key,
      o.key) && java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.kind,
      o.kind) && java.util.Objects.equals(
      this.computed,
      o.computed) && java.util.Objects.equals(
      this.shorthand,
      o.shorthand);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(key) + 3 * java.util.Objects.hashCode(value) + 5 * java.util.Objects.hashCode(kind) + 7 * java.util.Objects.hashCode(computed) + 11 * java.util.Objects.hashCode(shorthand);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Property other) {
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
      shorthand,
      other.shorthand);
  }

  public Property withKey(hydra.javaScript.syntax.Expression key) {
    return new Property(key, value, kind, computed, shorthand);
  }

  public Property withValue(hydra.javaScript.syntax.Expression value) {
    return new Property(key, value, kind, computed, shorthand);
  }

  public Property withKind(hydra.javaScript.syntax.PropertyKind kind) {
    return new Property(key, value, kind, computed, shorthand);
  }

  public Property withComputed(Boolean computed) {
    return new Property(key, value, kind, computed, shorthand);
  }

  public Property withShorthand(Boolean shorthand) {
    return new Property(key, value, kind, computed, shorthand);
  }
}
