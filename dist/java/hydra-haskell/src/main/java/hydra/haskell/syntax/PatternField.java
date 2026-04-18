// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A pattern field
 */
public class PatternField implements Serializable, Comparable<PatternField> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.PatternField");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  /**
   * The field name
   */
  public final hydra.haskell.syntax.Name name;

  /**
   * The field pattern
   */
  public final hydra.haskell.syntax.Pattern pattern;

  public PatternField (hydra.haskell.syntax.Name name, hydra.haskell.syntax.Pattern pattern) {
    this.name = name;
    this.pattern = pattern;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternField)) {
      return false;
    }
    PatternField o = (PatternField) other;
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
  public int compareTo(PatternField other) {
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

  public PatternField withName(hydra.haskell.syntax.Name name) {
    return new PatternField(name, pattern);
  }

  public PatternField withPattern(hydra.haskell.syntax.Pattern pattern) {
    return new PatternField(name, pattern);
  }
}
