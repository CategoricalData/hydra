package hydra.ext.haskell.ast;

public class PatternField {
  public final hydra.ext.haskell.ast.Name name;
  
  public final hydra.ext.haskell.ast.Pattern pattern;
  
  public PatternField (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Pattern pattern) {
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
  
  public PatternField withName(hydra.ext.haskell.ast.Name name) {
    return new PatternField(name, pattern);
  }
  
  public PatternField withPattern(hydra.ext.haskell.ast.Pattern pattern) {
    return new PatternField(name, pattern);
  }
}