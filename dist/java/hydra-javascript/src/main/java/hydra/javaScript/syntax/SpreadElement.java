// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A spread element (...x)
 */
public class SpreadElement implements Serializable, Comparable<SpreadElement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.SpreadElement");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.javaScript.syntax.Expression value;

  public SpreadElement (hydra.javaScript.syntax.Expression value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SpreadElement)) {
      return false;
    }
    SpreadElement o = (SpreadElement) other;
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
  public int compareTo(SpreadElement other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
