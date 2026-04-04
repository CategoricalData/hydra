// Note: this is an automatically generated file. Do not edit.

package hydra.error.packaging;

import java.io.Serializable;

/**
 * A definition whose name does not have the module's namespace as a prefix. If the module namespace is foo.bar, all definition names must have the form foo.bar.quux.
 */
public class DefinitionNotInModuleNamespaceError implements Serializable, Comparable<DefinitionNotInModuleNamespaceError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError");

  public static final hydra.core.Name NAMESPACE = new hydra.core.Name("namespace");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  /**
   * The namespace of the module
   */
  public final hydra.module.Namespace namespace;

  /**
   * The definition name that does not match the module namespace
   */
  public final hydra.core.Name name;

  public DefinitionNotInModuleNamespaceError (hydra.module.Namespace namespace, hydra.core.Name name) {
    this.namespace = namespace;
    this.name = name;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DefinitionNotInModuleNamespaceError)) {
      return false;
    }
    DefinitionNotInModuleNamespaceError o = (DefinitionNotInModuleNamespaceError) other;
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
  public int compareTo(DefinitionNotInModuleNamespaceError other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      namespace,
      other.namespace);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      name,
      other.name);
  }

  public DefinitionNotInModuleNamespaceError withNamespace(hydra.module.Namespace namespace) {
    return new DefinitionNotInModuleNamespaceError(namespace, name);
  }

  public DefinitionNotInModuleNamespaceError withName(hydra.core.Name name) {
    return new DefinitionNotInModuleNamespaceError(namespace, name);
  }
}
