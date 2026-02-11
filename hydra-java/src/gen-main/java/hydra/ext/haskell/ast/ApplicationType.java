// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An application type
 */
public class ApplicationType implements Serializable, Comparable<ApplicationType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ApplicationType");
  
  public static final hydra.core.Name FIELD_NAME_CONTEXT = new hydra.core.Name("context");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENT = new hydra.core.Name("argument");
  
  /**
   * The type being applied
   */
  public final hydra.ext.haskell.ast.Type context;
  
  /**
   * The type argument
   */
  public final hydra.ext.haskell.ast.Type argument;
  
  public ApplicationType (hydra.ext.haskell.ast.Type context, hydra.ext.haskell.ast.Type argument) {
    this.context = context;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationType)) {
      return false;
    }
    ApplicationType o = (ApplicationType) other;
    return java.util.Objects.equals(
      this.context,
      o.context) && java.util.Objects.equals(
      this.argument,
      o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(context) + 3 * java.util.Objects.hashCode(argument);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ApplicationType other) {
    int cmp = 0;
    cmp = ((Comparable) context).compareTo(other.context);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) argument).compareTo(other.argument);
  }
  
  public ApplicationType withContext(hydra.ext.haskell.ast.Type context) {
    return new ApplicationType(context, argument);
  }
  
  public ApplicationType withArgument(hydra.ext.haskell.ast.Type argument) {
    return new ApplicationType(context, argument);
  }
}
