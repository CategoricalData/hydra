// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.helpers;

import java.io.Serializable;

/**
 * Environment for Java code generation
 */
public class JavaEnvironment implements Serializable, Comparable<JavaEnvironment> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.helpers.JavaEnvironment");
  
  public static final hydra.core.Name FIELD_NAME_ALIASES = new hydra.core.Name("aliases");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_CONTEXT = new hydra.core.Name("typeContext");
  
  /**
   * Aliases and context state
   */
  public final hydra.ext.java.helpers.Aliases aliases;
  
  /**
   * Type context for type inference
   */
  public final hydra.typing.TypeContext typeContext;
  
  public JavaEnvironment (hydra.ext.java.helpers.Aliases aliases, hydra.typing.TypeContext typeContext) {
    this.aliases = aliases;
    this.typeContext = typeContext;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof JavaEnvironment)) {
      return false;
    }
    JavaEnvironment o = (JavaEnvironment) other;
    return java.util.Objects.equals(
      this.aliases,
      o.aliases) && java.util.Objects.equals(
      this.typeContext,
      o.typeContext);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(aliases) + 3 * java.util.Objects.hashCode(typeContext);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(JavaEnvironment other) {
    int cmp = 0;
    cmp = ((Comparable) aliases).compareTo(other.aliases);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) typeContext).compareTo(other.typeContext);
  }
  
  public JavaEnvironment withAliases(hydra.ext.java.helpers.Aliases aliases) {
    return new JavaEnvironment(aliases, typeContext);
  }
  
  public JavaEnvironment withTypeContext(hydra.typing.TypeContext typeContext) {
    return new JavaEnvironment(aliases, typeContext);
  }
}
