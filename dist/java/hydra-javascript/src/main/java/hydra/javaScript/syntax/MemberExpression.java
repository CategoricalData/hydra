// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A member access expression
 */
public class MemberExpression implements Serializable, Comparable<MemberExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.MemberExpression");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name COMPUTED = new hydra.core.Name("computed");

  public static final hydra.core.Name OPTIONAL = new hydra.core.Name("optional");

  /**
   * The object
   */
  public final hydra.javaScript.syntax.Expression object;

  /**
   * The property
   */
  public final hydra.javaScript.syntax.Expression property;

  /**
   * Whether using bracket notation (obj[prop])
   */
  public final Boolean computed;

  /**
   * Whether using optional chaining (?.)
   */
  public final Boolean optional;

  public MemberExpression (hydra.javaScript.syntax.Expression object, hydra.javaScript.syntax.Expression property, Boolean computed, Boolean optional) {
    this.object = object;
    this.property = property;
    this.computed = computed;
    this.optional = optional;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MemberExpression)) {
      return false;
    }
    MemberExpression o = (MemberExpression) other;
    return java.util.Objects.equals(
      this.object,
      o.object) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.computed,
      o.computed) && java.util.Objects.equals(
      this.optional,
      o.optional);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(object) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(computed) + 7 * java.util.Objects.hashCode(optional);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MemberExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      object,
      other.object);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
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
      optional,
      other.optional);
  }

  public MemberExpression withObject(hydra.javaScript.syntax.Expression object) {
    return new MemberExpression(object, property, computed, optional);
  }

  public MemberExpression withProperty(hydra.javaScript.syntax.Expression property) {
    return new MemberExpression(object, property, computed, optional);
  }

  public MemberExpression withComputed(Boolean computed) {
    return new MemberExpression(object, property, computed, optional);
  }

  public MemberExpression withOptional(Boolean optional) {
    return new MemberExpression(object, property, computed, optional);
  }
}
