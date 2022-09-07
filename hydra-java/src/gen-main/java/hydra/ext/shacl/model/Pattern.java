package hydra.ext.shacl.model;

/**
 * A SHACL pattern. See https://www.w3.org/TR/shacl/#PatternConstraintComponent
 */
public class Pattern {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.Pattern");
  
  public final String regex;
  
  public final java.util.Optional<String> flags;
  
  public Pattern (String regex, java.util.Optional<String> flags) {
    this.regex = regex;
    this.flags = flags;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern)) {
      return false;
    }
    Pattern o = (Pattern) (other);
    return regex.equals(o.regex) && flags.equals(o.flags);
  }
  
  @Override
  public int hashCode() {
    return 2 * regex.hashCode() + 3 * flags.hashCode();
  }
  
  public Pattern withRegex(String regex) {
    return new Pattern(regex, flags);
  }
  
  public Pattern withFlags(java.util.Optional<String> flags) {
    return new Pattern(regex, flags);
  }
}