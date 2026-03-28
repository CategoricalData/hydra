// Note: this is an automatically generated file. Do not edit.

package hydra.error.packaging;

import java.io.Serializable;

/**
 * Two or more definitions in the same module share the same name
 */
public class DuplicateDefinitionNameError implements Serializable, Comparable<DuplicateDefinitionNameError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError");

  public static final hydra.core.Name NAMESPACE = new hydra.core.Name("namespace");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The namespace of the module containing the duplicates
   */
  public final hydra.module.Namespace namespace;

  /**
   * The duplicated definition name
   */
  public final hydra.core.Name name;

  public DuplicateDefinitionNameError (hydra.module.Namespace namespace, hydra.core.Name name) {
    this.namespace = namespace;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DuplicateDefinitionNameError)) {
      return false;
    }
    DuplicateDefinitionNameError o = (DuplicateDefinitionNameError) other;
    return java.util.Objects.equals(
      this.namespace,
      o.namespace) && java.util.Objects.equals(
      this.name,
      o.name);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespace) + 3 * java.util.Objects.hashCode(name);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DuplicateDefinitionNameError other) {
    int cmp = 0;
    cmp = ((Comparable) namespace).compareTo(other.namespace);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) name).compareTo(other.name);
  }

  public DuplicateDefinitionNameError withNamespace(hydra.module.Namespace namespace) {
    return new DuplicateDefinitionNameError(namespace, name);
  }

  public DuplicateDefinitionNameError withName(hydra.core.Name name) {
    return new DuplicateDefinitionNameError(namespace, name);
  }
}
