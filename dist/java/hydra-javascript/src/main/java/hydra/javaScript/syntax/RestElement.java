// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A rest element pattern (...x)
 */
public class RestElement implements Serializable, Comparable<RestElement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.RestElement");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.javaScript.syntax.Pattern value;

  public RestElement (hydra.javaScript.syntax.Pattern value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RestElement)) {
      return false;
    }
    RestElement o = (RestElement) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RestElement other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
