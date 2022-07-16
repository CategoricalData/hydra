package hydra.ext.haskell.ast;

public class PatternField {
  public final Name name;
  
  public final Pattern pattern;
  
  public PatternField (Name name, Pattern pattern) {
    this.name = name;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PatternField)) {
      return false;
    }
    PatternField o = (PatternField) (other);
    return name.equals(o.name) && pattern.equals(o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * pattern.hashCode();
  }
  
  public PatternField withName(Name name) {
    return new PatternField(name, pattern);
  }
  
  public PatternField withPattern(Pattern pattern) {
    return new PatternField(name, pattern);
  }
}