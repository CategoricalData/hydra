// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A documentation string
 */
public class Docstring implements Serializable, Comparable<Docstring> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.Docstring");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public Docstring (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Docstring)) {
      return false;
    }
    Docstring o = (Docstring) other;
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
  public int compareTo(Docstring other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
