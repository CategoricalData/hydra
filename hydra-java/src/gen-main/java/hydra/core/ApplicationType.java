// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * The type-level analog of an application term
 */
public class ApplicationType implements Serializable, Comparable<ApplicationType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.ApplicationType");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENT = new hydra.core.Name("argument");
  
  /**
   * The left-hand side of the application
   */
  public final hydra.core.Type function;
  
  /**
   * The right-hand side of the application
   */
  public final hydra.core.Type argument;
  
  public ApplicationType (hydra.core.Type function, hydra.core.Type argument) {
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationType)) {
      return false;
    }
    ApplicationType o = (ApplicationType) other;
    return java.util.Objects.equals(
      this.function,
      o.function) && java.util.Objects.equals(
      this.argument,
      o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(function) + 3 * java.util.Objects.hashCode(argument);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ApplicationType other) {
    int cmp = 0;
    cmp = ((Comparable) function).compareTo(other.function);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) argument).compareTo(other.argument);
  }
  
  public ApplicationType withFunction(hydra.core.Type function) {
    return new ApplicationType(function, argument);
  }
  
  public ApplicationType withArgument(hydra.core.Type argument) {
    return new ApplicationType(function, argument);
  }
}
