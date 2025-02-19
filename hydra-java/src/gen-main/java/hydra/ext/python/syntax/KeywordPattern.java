// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class KeywordPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.KeywordPattern");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.ext.python.syntax.Pattern pattern;
  
  public KeywordPattern (hydra.ext.python.syntax.Name name, hydra.ext.python.syntax.Pattern pattern) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((pattern));
    this.name = name;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof KeywordPattern)) {
      return false;
    }
    KeywordPattern o = (KeywordPattern) (other);
    return name.equals(o.name) && pattern.equals(o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * pattern.hashCode();
  }
  
  public KeywordPattern withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new KeywordPattern(name, pattern);
  }
  
  public KeywordPattern withPattern(hydra.ext.python.syntax.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new KeywordPattern(name, pattern);
  }
}