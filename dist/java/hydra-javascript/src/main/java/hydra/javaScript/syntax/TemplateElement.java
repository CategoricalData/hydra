// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A static part of a template literal
 */
public class TemplateElement implements Serializable, Comparable<TemplateElement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.TemplateElement");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name TAIL = new hydra.core.Name("tail");

  /**
   * The raw string value
   */
  public final String value;

  /**
   * Whether this is the last element
   */
  public final Boolean tail;

  public TemplateElement (String value, Boolean tail) {
    this.value = value;
    this.tail = tail;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TemplateElement)) {
      return false;
    }
    TemplateElement o = (TemplateElement) other;
    return java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.tail,
      o.tail);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value) + 3 * java.util.Objects.hashCode(tail);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TemplateElement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      value,
      other.value);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      tail,
      other.tail);
  }

  public TemplateElement withValue(String value) {
    return new TemplateElement(value, tail);
  }

  public TemplateElement withTail(Boolean tail) {
    return new TemplateElement(value, tail);
  }
}
