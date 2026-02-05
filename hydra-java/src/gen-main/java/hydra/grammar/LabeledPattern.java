// Note: this is an automatically generated file. Do not edit.

package hydra.grammar;

import java.io.Serializable;

/**
 * A pattern together with a name (label)
 */
public class LabeledPattern implements Serializable, Comparable<LabeledPattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.grammar.LabeledPattern");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  /**
   * The label for the pattern
   */
  public final hydra.grammar.Label label;
  
  /**
   * The pattern being labeled
   */
  public final hydra.grammar.Pattern pattern;
  
  public LabeledPattern (hydra.grammar.Label label, hydra.grammar.Pattern pattern) {
    this.label = label;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledPattern)) {
      return false;
    }
    LabeledPattern o = (LabeledPattern) (other);
    return java.util.Objects.equals(
      this.label,
      o.label) && java.util.Objects.equals(
      this.pattern,
      o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(label) + 3 * java.util.Objects.hashCode(pattern);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LabeledPattern other) {
    int cmp = 0;
    cmp = ((Comparable) (label)).compareTo(other.label);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (pattern)).compareTo(other.pattern);
  }
  
  public LabeledPattern withLabel(hydra.grammar.Label label) {
    return new LabeledPattern(label, pattern);
  }
  
  public LabeledPattern withPattern(hydra.grammar.Pattern pattern) {
    return new LabeledPattern(label, pattern);
  }
}
