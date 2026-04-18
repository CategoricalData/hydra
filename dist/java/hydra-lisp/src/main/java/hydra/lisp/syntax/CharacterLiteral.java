// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A character literal. Concrete syntax varies: \a (Clojure), ?a (Emacs Lisp), #\a (Common Lisp, Scheme)
 */
public class CharacterLiteral implements Serializable, Comparable<CharacterLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.CharacterLiteral");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  /**
   * The character value
   */
  public final String value;

  public CharacterLiteral (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CharacterLiteral)) {
      return false;
    }
    CharacterLiteral o = (CharacterLiteral) other;
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
  public int compareTo(CharacterLiteral other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
