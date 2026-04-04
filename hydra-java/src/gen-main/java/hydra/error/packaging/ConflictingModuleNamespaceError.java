// Note: this is an automatically generated file. Do not edit.

package hydra.error.packaging;

import java.io.Serializable;

/**
 * A module namespace which, when mapped to a target language's directory or package structure, conflicts with another module's mapped namespace. For example, hydra.foo.bar and hydra.fooBar might both map to the same directory in a case-insensitive filesystem.
 */
public class ConflictingModuleNamespaceError implements Serializable, Comparable<ConflictingModuleNamespaceError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError");

  public static final hydra.core.Name FIRST = new hydra.core.Name("first");

  public static final hydra.core.Name SECOND = new hydra.core.Name("second");

  /**
   * The first module namespace
   */
  public final hydra.packaging.Namespace first;

  /**
   * The second module namespace that conflicts with the first
   */
  public final hydra.packaging.Namespace second;

  public ConflictingModuleNamespaceError (hydra.packaging.Namespace first, hydra.packaging.Namespace second) {
    this.first = first;
    this.second = second;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConflictingModuleNamespaceError)) {
      return false;
    }
    ConflictingModuleNamespaceError o = (ConflictingModuleNamespaceError) other;
    return java.util.Objects.equals(
      this.first,
      o.first) && java.util.Objects.equals(
      this.second,
      o.second);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(first) + 3 * java.util.Objects.hashCode(second);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ConflictingModuleNamespaceError other) {
    int cmp = 0;
    cmp = ((Comparable) first).compareTo(other.first);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) second).compareTo(other.second);
  }

  public ConflictingModuleNamespaceError withFirst(hydra.packaging.Namespace first) {
    return new ConflictingModuleNamespaceError(first, second);
  }

  public ConflictingModuleNamespaceError withSecond(hydra.packaging.Namespace second) {
    return new ConflictingModuleNamespaceError(first, second);
  }
}
