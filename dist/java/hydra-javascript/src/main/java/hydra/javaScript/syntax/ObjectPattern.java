// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An object destructuring pattern {a, b: c}
 */
public class ObjectPattern implements Serializable, Comparable<ObjectPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ObjectPattern");

  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");

  /**
   * The property patterns
   */
  public final java.util.List<hydra.javaScript.syntax.ObjectPatternProperty> properties;

  public ObjectPattern (java.util.List<hydra.javaScript.syntax.ObjectPatternProperty> properties) {
    this.properties = properties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectPattern)) {
      return false;
    }
    ObjectPattern o = (ObjectPattern) other;
    return java.util.Objects.equals(
      this.properties,
      o.properties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(properties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ObjectPattern other) {
    return hydra.util.Comparing.compare(
      properties,
      other.properties);
  }
}
