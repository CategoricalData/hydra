// Note: this is an automatically generated file. Do not edit.

package hydra.python.syntax;

import java.io.Serializable;

public class KeywordPattern implements Serializable, Comparable<KeywordPattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.python.syntax.KeywordPattern");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  public final hydra.python.syntax.Name name;

  public final hydra.python.syntax.Pattern pattern;

  public KeywordPattern (hydra.python.syntax.Name name, hydra.python.syntax.Pattern pattern) {
    this.name = name;
    this.pattern = pattern;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof KeywordPattern)) {
      return false;
    }
    KeywordPattern o = (KeywordPattern) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.pattern,
      o.pattern);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(pattern);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(KeywordPattern other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      pattern,
      other.pattern);
  }

  public KeywordPattern withName(hydra.python.syntax.Name name) {
    return new KeywordPattern(name, pattern);
  }

  public KeywordPattern withPattern(hydra.python.syntax.Pattern pattern) {
    return new KeywordPattern(name, pattern);
  }
}
