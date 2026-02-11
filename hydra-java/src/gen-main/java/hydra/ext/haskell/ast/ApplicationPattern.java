// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An application pattern
 */
public class ApplicationPattern implements Serializable, Comparable<ApplicationPattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ApplicationPattern");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  /**
   * The constructor name
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * The pattern arguments
   */
  public final java.util.List<hydra.ext.haskell.ast.Pattern> args;
  
  public ApplicationPattern (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.Pattern> args) {
    this.name = name;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationPattern)) {
      return false;
    }
    ApplicationPattern o = (ApplicationPattern) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.args,
      o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(args);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ApplicationPattern other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      args.hashCode(),
      other.args.hashCode());
  }
  
  public ApplicationPattern withName(hydra.ext.haskell.ast.Name name) {
    return new ApplicationPattern(name, args);
  }
  
  public ApplicationPattern withArgs(java.util.List<hydra.ext.haskell.ast.Pattern> args) {
    return new ApplicationPattern(name, args);
  }
}
