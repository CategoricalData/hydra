// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A logical collection of elements in the same namespace, having dependencies on zero or more other modules
 */
public class Module implements Serializable, Comparable<Module> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.module.Module");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE = new hydra.core.Name("namespace");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENTS = new hydra.core.Name("elements");
  
  public static final hydra.core.Name FIELD_NAME_TERM_DEPENDENCIES = new hydra.core.Name("termDependencies");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_DEPENDENCIES = new hydra.core.Name("typeDependencies");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  /**
   * A common prefix for all element names in the module
   */
  public final hydra.module.Namespace namespace;
  
  /**
   * The elements defined in this module
   */
  public final java.util.List<hydra.core.Binding> elements;
  
  /**
   * Any modules which the term expressions of this module directly depend upon
   */
  public final java.util.List<hydra.module.Namespace> termDependencies;
  
  /**
   * Any modules which the type expressions of this module directly depend upon
   */
  public final java.util.List<hydra.module.Namespace> typeDependencies;
  
  /**
   * An optional human-readable description of the module
   */
  public final hydra.util.Maybe<String> description;
  
  public Module (hydra.module.Namespace namespace, java.util.List<hydra.core.Binding> elements, java.util.List<hydra.module.Namespace> termDependencies, java.util.List<hydra.module.Namespace> typeDependencies, hydra.util.Maybe<String> description) {
    this.namespace = namespace;
    this.elements = elements;
    this.termDependencies = termDependencies;
    this.typeDependencies = typeDependencies;
    this.description = description;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Module)) {
      return false;
    }
    Module o = (Module) (other);
    return java.util.Objects.equals(
      this.namespace,
      o.namespace) && java.util.Objects.equals(
      this.elements,
      o.elements) && java.util.Objects.equals(
      this.termDependencies,
      o.termDependencies) && java.util.Objects.equals(
      this.typeDependencies,
      o.typeDependencies) && java.util.Objects.equals(
      this.description,
      o.description);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespace) + 3 * java.util.Objects.hashCode(elements) + 5 * java.util.Objects.hashCode(termDependencies) + 7 * java.util.Objects.hashCode(typeDependencies) + 11 * java.util.Objects.hashCode(description);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Module other) {
    int cmp = 0;
    cmp = ((Comparable) (namespace)).compareTo(other.namespace);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      elements.hashCode(),
      other.elements.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      termDependencies.hashCode(),
      other.termDependencies.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      typeDependencies.hashCode(),
      other.typeDependencies.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      description.hashCode(),
      other.description.hashCode());
  }
  
  public Module withNamespace(hydra.module.Namespace namespace) {
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
  
  public Module withElements(java.util.List<hydra.core.Binding> elements) {
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
  
  public Module withTermDependencies(java.util.List<hydra.module.Namespace> termDependencies) {
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
  
  public Module withTypeDependencies(java.util.List<hydra.module.Namespace> typeDependencies) {
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
  
  public Module withDescription(hydra.util.Maybe<String> description) {
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
}
