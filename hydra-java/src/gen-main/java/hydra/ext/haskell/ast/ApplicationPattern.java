// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class ApplicationPattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ApplicationPattern");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final java.util.List<hydra.ext.haskell.ast.Pattern> args;
  
  public ApplicationPattern (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.Pattern> args) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((args));
    this.name = name;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationPattern)) {
      return false;
    }
    ApplicationPattern o = (ApplicationPattern) (other);
    return name.equals(o.name) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * args.hashCode();
  }
  
  public ApplicationPattern withName(hydra.ext.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ApplicationPattern(name, args);
  }
  
  public ApplicationPattern withArgs(java.util.List<hydra.ext.haskell.ast.Pattern> args) {
    java.util.Objects.requireNonNull((args));
    return new ApplicationPattern(name, args);
  }
}