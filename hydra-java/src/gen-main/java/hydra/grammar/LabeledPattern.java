// Note: this is an automatically generated file. Do not edit.

package hydra.grammar;

import java.io.Serializable;

/**
 * A pattern together with a name (label)
 */
public class LabeledPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.grammar.LabeledPattern");
  
  public static final hydra.core.Name FIELD_NAME_LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public final hydra.grammar.Label label;
  
  public final hydra.grammar.Pattern pattern;
  
  public LabeledPattern (hydra.grammar.Label label, hydra.grammar.Pattern pattern) {
    java.util.Objects.requireNonNull((label));
    java.util.Objects.requireNonNull((pattern));
    this.label = label;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LabeledPattern)) {
      return false;
    }
    LabeledPattern o = (LabeledPattern) (other);
    return label.equals(o.label) && pattern.equals(o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * pattern.hashCode();
  }
  
  public LabeledPattern withLabel(hydra.grammar.Label label) {
    java.util.Objects.requireNonNull((label));
    return new LabeledPattern(label, pattern);
  }
  
  public LabeledPattern withPattern(hydra.grammar.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new LabeledPattern(label, pattern);
  }
}
