// Note: this is an automatically generated file. Do not edit.

package hydra.module;

import java.io.Serializable;

/**
 * A logical collection of elements in the same namespace, having dependencies on zero or more other modules
 */
public class Module implements Serializable {
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
    java.util.Objects.requireNonNull((namespace));
    java.util.Objects.requireNonNull((elements));
    java.util.Objects.requireNonNull((termDependencies));
    java.util.Objects.requireNonNull((typeDependencies));
    java.util.Objects.requireNonNull((description));
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
    return namespace.equals(o.namespace) && elements.equals(o.elements) && termDependencies.equals(o.termDependencies) && typeDependencies.equals(o.typeDependencies) && description.equals(o.description);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespace.hashCode() + 3 * elements.hashCode() + 5 * termDependencies.hashCode() + 7 * typeDependencies.hashCode() + 11 * description.hashCode();
  }
  
  public Module withNamespace(hydra.module.Namespace namespace) {
    java.util.Objects.requireNonNull((namespace));
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
  
  public Module withElements(java.util.List<hydra.core.Binding> elements) {
    java.util.Objects.requireNonNull((elements));
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
  
  public Module withTermDependencies(java.util.List<hydra.module.Namespace> termDependencies) {
    java.util.Objects.requireNonNull((termDependencies));
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
  
  public Module withTypeDependencies(java.util.List<hydra.module.Namespace> typeDependencies) {
    java.util.Objects.requireNonNull((typeDependencies));
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
  
  public Module withDescription(hydra.util.Maybe<String> description) {
    java.util.Objects.requireNonNull((description));
    return new Module(namespace, elements, termDependencies, typeDependencies, description);
  }
}
