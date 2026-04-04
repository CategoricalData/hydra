// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * A class assertion
 */
public class ClassAssertion implements Serializable, Comparable<ClassAssertion> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.ClassAssertion");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPES = new hydra.core.Name("types");

  /**
   * The name of the class
   */
  public final hydra.ext.haskell.syntax.Name name;

  /**
   * The types to which the class is applied
   */
  public final java.util.List<hydra.ext.haskell.syntax.Type> types;

  public ClassAssertion (hydra.ext.haskell.syntax.Name name, java.util.List<hydra.ext.haskell.syntax.Type> types) {
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
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      types,
      other.types);
  }

  public ClassAssertion withName(hydra.ext.haskell.syntax.Name name) {
    return new ClassAssertion(name, types);
  }

  public ClassAssertion withTypes(java.util.List<hydra.ext.haskell.syntax.Type> types) {
    return new ClassAssertion(name, types);
  }
}
