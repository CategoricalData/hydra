// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class TPrimaryAndArguments implements Serializable, Comparable<TPrimaryAndArguments> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndArguments");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.python.syntax.TPrimary primary;
  
  public final hydra.util.Maybe<hydra.ext.python.syntax.Args> arguments;
  
  public TPrimaryAndArguments (hydra.ext.python.syntax.TPrimary primary, hydra.util.Maybe<hydra.ext.python.syntax.Args> arguments) {
    this.primary = primary;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TPrimaryAndArguments)) {
      return false;
    }
    TPrimaryAndArguments o = (TPrimaryAndArguments) other;
    return java.util.Objects.equals(
      this.primary,
      o.primary) && java.util.Objects.equals(
      this.arguments,
      o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(primary) + 3 * java.util.Objects.hashCode(arguments);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TPrimaryAndArguments other) {
    int cmp = 0;
    cmp = ((Comparable) primary).compareTo(other.primary);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      arguments.hashCode(),
      other.arguments.hashCode());
  }
  
  public TPrimaryAndArguments withPrimary(hydra.ext.python.syntax.TPrimary primary) {
    return new TPrimaryAndArguments(primary, arguments);
  }
  
  public TPrimaryAndArguments withArguments(hydra.util.Maybe<hydra.ext.python.syntax.Args> arguments) {
    return new TPrimaryAndArguments(primary, arguments);
  }
}
