package hydra.grammar;

public class LabeledPattern {
  public final hydra.grammar.Label label;
  
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
    return label.equals(o.label) && pattern.equals(o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * label.hashCode() + 3 * pattern.hashCode();
  }
  
  public LabeledPattern withLabel(hydra.grammar.Label label) {
    return new LabeledPattern(label, pattern);
  }
  
  public LabeledPattern withPattern(hydra.grammar.Pattern pattern) {
    return new LabeledPattern(label, pattern);
  }
}