// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A class assertion
 */
public class ClassAssertion implements Serializable, Comparable<ClassAssertion> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ClassAssertion");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPES = new hydra.core.Name("types");
  
  /**
   * The name of the class
   */
  public final hydra.ext.haskell.ast.Name name;
  
  /**
   * The types to which the class is applied
   */
  public final java.util.List<hydra.ext.haskell.ast.Type> types;
  
  public ClassAssertion (hydra.ext.haskell.ast.Name name, java.util.List<hydra.ext.haskell.ast.Type> types) {
    this.name = name;
    this.types = types;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassAssertion)) {
      return false;
    }
    ClassAssertion o = (ClassAssertion) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.types,
      o.types);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(types);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClassAssertion other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      types.hashCode(),
      other.types.hashCode());
  }
  
  public ClassAssertion withName(hydra.ext.haskell.ast.Name name) {
    return new ClassAssertion(name, types);
  }
  
  public ClassAssertion withTypes(java.util.List<hydra.ext.haskell.ast.Type> types) {
    return new ClassAssertion(name, types);
  }
}
