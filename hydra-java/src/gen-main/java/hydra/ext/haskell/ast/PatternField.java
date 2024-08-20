// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class PatternField implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.PatternField");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final hydra.ext.haskell.ast.Pattern pattern;
  
  public PatternField (hydra.ext.haskell.ast.Name name, hydra.ext.haskell.ast.Pattern pattern) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((pattern));
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
    java.util.Objects.requireNonNull((name));
    return new PatternField(name, pattern);
  }
  
  public PatternField withPattern(hydra.ext.haskell.ast.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new PatternField(name, pattern);
  }
}
