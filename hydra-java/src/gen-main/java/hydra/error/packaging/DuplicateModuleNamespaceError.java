// Note: this is an automatically generated file. Do not edit.

package hydra.error.packaging;

import java.io.Serializable;

/**
 * Two or more modules in the same package share the same namespace
 */
public class DuplicateModuleNamespaceError implements Serializable, Comparable<DuplicateModuleNamespaceError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.packaging.DuplicateModuleNamespaceError");

  public static final hydra.core.Name NAMESPACE = new hydra.core.Name("namespace");

  /**
   * The duplicated module namespace
   */
  public final hydra.packaging.Namespace namespace;

  public DuplicateModuleNamespaceError (hydra.packaging.Namespace namespace) {
    this.namespace = namespace;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DuplicateModuleNamespaceError)) {
      return false;
    }
    DuplicateModuleNamespaceError o = (DuplicateModuleNamespaceError) other;
    return java.util.Objects.equals(
      this.namespace,
      o.namespace);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespace);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DuplicateModuleNamespaceError other) {
    return ((Comparable) namespace).compareTo(other.namespace);
  }
}
