// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.shacl.model;

import java.io.Serializable;

/**
 * A SHACL pattern. See https://www.w3.org/TR/shacl/#PatternConstraintComponent
 */
public class Pattern implements Serializable, Comparable<Pattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.shacl.model.Pattern");
  
  public static final hydra.core.Name REGEX = new hydra.core.Name("regex");
  
  public static final hydra.core.Name FLAGS = new hydra.core.Name("flags");
  
  public final String regex;
  
  public final hydra.util.Maybe<String> flags;
  
  public Pattern (String regex, hydra.util.Maybe<String> flags) {
    this.regex = regex;
    this.flags = flags;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern)) {
      return false;
    }
    Pattern o = (Pattern) other;
    return java.util.Objects.equals(
      this.regex,
      o.regex) && java.util.Objects.equals(
      this.flags,
      o.flags);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(regex) + 3 * java.util.Objects.hashCode(flags);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pattern other) {
    int cmp = 0;
    cmp = ((Comparable) regex).compareTo(other.regex);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      flags.hashCode(),
      other.flags.hashCode());
  }
  
  public Pattern withRegex(String regex) {
    return new Pattern(regex, flags);
  }
  
  public Pattern withFlags(hydra.util.Maybe<String> flags) {
    return new Pattern(regex, flags);
  }
}
