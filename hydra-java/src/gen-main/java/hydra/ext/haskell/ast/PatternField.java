// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A pattern field
 */
public class PatternField implements Serializable, Comparable<PatternField> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.PatternField");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  /**
   * The field name
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * The field pattern
   */
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
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) pattern).compareTo(other.pattern);
  }
  
  public PatternField withName(hydra.ext.haskell.ast.Name name) {
    return new PatternField(name, pattern);
  }
  
  public PatternField withPattern(hydra.ext.haskell.ast.Pattern pattern) {
    return new PatternField(name, pattern);
  }
}
