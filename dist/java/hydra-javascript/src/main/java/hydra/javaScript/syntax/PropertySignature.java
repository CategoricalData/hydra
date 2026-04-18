// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A property signature in an object type
 */
public class PropertySignature implements Serializable, Comparable<PropertySignature> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.PropertySignature");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name OPTIONAL = new hydra.core.Name("optional");

  public static final hydra.core.Name READONLY = new hydra.core.Name("readonly");

  /**
   * Property name
   */
  public final hydra.javaScript.syntax.Identifier name;

  /**
   * Property type
   */
  public final hydra.javaScript.syntax.TypeExpression type;

  /**
   * Whether the property is optional
   */
  public final Boolean optional;

  /**
   * Whether the property is readonly
   */
  public final Boolean readonly;

  public PropertySignature (hydra.javaScript.syntax.Identifier name, hydra.javaScript.syntax.TypeExpression type, Boolean optional, Boolean readonly) {
    this.name = name;
    this.type = type;
    this.optional = optional;
    this.readonly = readonly;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertySignature)) {
      return false;
    }
    PropertySignature o = (PropertySignature) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.optional,
      o.optional) && java.util.Objects.equals(
      this.readonly,
      o.readonly);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type) + 5 * java.util.Objects.hashCode(optional) + 7 * java.util.Objects.hashCode(readonly);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertySignature other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      optional,
      other.optional);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      readonly,
      other.readonly);
  }

  public PropertySignature withName(hydra.javaScript.syntax.Identifier name) {
    return new PropertySignature(name, type, optional, readonly);
  }

  public PropertySignature withType(hydra.javaScript.syntax.TypeExpression type) {
    return new PropertySignature(name, type, optional, readonly);
  }

  public PropertySignature withOptional(Boolean optional) {
    return new PropertySignature(name, type, optional, readonly);
  }

  public PropertySignature withReadonly(Boolean readonly) {
    return new PropertySignature(name, type, optional, readonly);
  }
}
