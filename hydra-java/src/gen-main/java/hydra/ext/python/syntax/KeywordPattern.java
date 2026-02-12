// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class KeywordPattern implements Serializable, Comparable<KeywordPattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.KeywordPattern");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public final hydra.ext.python.syntax.Name name;
  
  public final hydra.ext.python.syntax.Pattern pattern;
  
  public KeywordPattern (hydra.ext.python.syntax.Name name, hydra.ext.python.syntax.Pattern pattern) {
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
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) pattern).compareTo(other.pattern);
  }
  
  public KeywordPattern withName(hydra.ext.python.syntax.Name name) {
    return new KeywordPattern(name, pattern);
  }
  
  public KeywordPattern withPattern(hydra.ext.python.syntax.Pattern pattern) {
    return new KeywordPattern(name, pattern);
  }
}
