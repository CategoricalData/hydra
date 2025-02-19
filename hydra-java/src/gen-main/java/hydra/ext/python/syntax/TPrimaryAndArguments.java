// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TPrimaryAndArguments implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndArguments");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.python.syntax.TPrimary primary;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Args> arguments;
  
  public TPrimaryAndArguments (hydra.ext.python.syntax.TPrimary primary, hydra.util.Opt<hydra.ext.python.syntax.Args> arguments) {
    java.util.Objects.requireNonNull((primary));
    java.util.Objects.requireNonNull((arguments));
    this.primary = primary;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TPrimaryAndArguments)) {
      return false;
    }
    TPrimaryAndArguments o = (TPrimaryAndArguments) (other);
    return primary.equals(o.primary) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * arguments.hashCode();
  }
  
  public TPrimaryAndArguments withPrimary(hydra.ext.python.syntax.TPrimary primary) {
    java.util.Objects.requireNonNull((primary));
    return new TPrimaryAndArguments(primary, arguments);
  }
  
  public TPrimaryAndArguments withArguments(hydra.util.Opt<hydra.ext.python.syntax.Args> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new TPrimaryAndArguments(primary, arguments);
  }
}