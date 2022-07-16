package hydra.grammar;

public class LabeledPattern {
  public final Label label;
  
  public final Pattern pattern;
  
  public LabeledPattern (Label label, Pattern pattern) {
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
  
  public LabeledPattern withLabel(Label label) {
    return new LabeledPattern(label, pattern);
  }
  
  public LabeledPattern withPattern(Pattern pattern) {
    return new LabeledPattern(label, pattern);
  }
}