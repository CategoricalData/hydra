// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class Pattern_Application implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.Pattern.Application");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  public final hydra.ext.haskell.ast.Name name;
  
  public final java.util.List<hydra.ext.haskell.ast.Pattern> args;
  
  public Pattern_Application (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.Pattern> args) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((args));
    this.name = name;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pattern_Application)) {
      return false;
    }
    Pattern_Application o = (Pattern_Application) (other);
    return name.equals(o.name) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * args.hashCode();
  }
  
  public Pattern_Application withName(hydra.ext.haskell.ast.Name name) {
    java.util.Objects.requireNonNull((name));
    return new Pattern_Application(name, args);
  }
  
  public Pattern_Application withArgs(java.util.List<hydra.ext.haskell.ast.Pattern> args) {
    java.util.Objects.requireNonNull((args));
    return new Pattern_Application(name, args);
  }
}
