// Note: this is an automatically generated file. Do not edit.

package hydra.packaging;

import java.io.Serializable;

/**
 * A logical collection of elements in the same namespace, having dependencies on zero or more other modules
 */
public class Module implements Serializable, Comparable<Module> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.packaging.Module");

  public static final hydra.core.Name NAMESPACE = new hydra.core.Name("namespace");

  public static final hydra.core.Name DEFINITIONS = new hydra.core.Name("definitions");

  public static final hydra.core.Name TERM_DEPENDENCIES = new hydra.core.Name("termDependencies");

  public static final hydra.core.Name TYPE_DEPENDENCIES = new hydra.core.Name("typeDependencies");

  public static final hydra.core.Name DESCRIPTION = new hydra.core.Name("description");

  /**
   * A common prefix for all element names in the module
   */
  public final hydra.packaging.Namespace namespace;

  /**
   * The definitions in this module
   */
  public final java.util.List<hydra.packaging.Definition> definitions;

  /**
   * Any modules which the term expressions of this module directly depend upon
   */
  public final java.util.List<hydra.packaging.Namespace> termDependencies;

  /**
   * Any modules which the type expressions of this module directly depend upon
   */
  public final java.util.List<hydra.packaging.Namespace> typeDependencies;

  /**
   * An optional human-readable description of the module
   */
  public final hydra.util.Maybe<String> description;

  public Module (hydra.packaging.Namespace namespace, java.util.List<hydra.packaging.Definition> definitions, java.util.List<hydra.packaging.Namespace> termDependencies, java.util.List<hydra.packaging.Namespace> typeDependencies, hydra.util.Maybe<String> description) {
    this.namespace = namespace;
    this.definitions = definitions;
    this.termDependencies = termDependencies;
    this.typeDependencies = typeDependencies;
    this.description = description;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Module)) {
      return false;
    }
    Module o = (Module) other;
    return java.util.Objects.equals(
      this.namespace,
      o.namespace) && java.util.Objects.equals(
      this.definitions,
      o.definitions) && java.util.Objects.equals(
      this.termDependencies,
      o.termDependencies) && java.util.Objects.equals(
      this.typeDependencies,
      o.typeDependencies) && java.util.Objects.equals(
      this.description,
      o.description);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespace) + 3 * java.util.Objects.hashCode(definitions) + 5 * java.util.Objects.hashCode(termDependencies) + 7 * java.util.Objects.hashCode(typeDependencies) + 11 * java.util.Objects.hashCode(description);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Module other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      namespace,
      other.namespace);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      definitions,
      other.definitions);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      termDependencies,
      other.termDependencies);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      typeDependencies,
      other.typeDependencies);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      description,
      other.description);
  }

  public Module withNamespace(hydra.packaging.Namespace namespace) {
    return new Module(namespace, definitions, termDependencies, typeDependencies, description);
  }

  public Module withDefinitions(java.util.List<hydra.packaging.Definition> definitions) {
    return new Module(namespace, definitions, termDependencies, typeDependencies, description);
  }

  public Module withTermDependencies(java.util.List<hydra.packaging.Namespace> termDependencies) {
    return new Module(namespace, definitions, termDependencies, typeDependencies, description);
  }

  public Module withTypeDependencies(java.util.List<hydra.packaging.Namespace> typeDependencies) {
    return new Module(namespace, definitions, termDependencies, typeDependencies, description);
  }

  public Module withDescription(hydra.util.Maybe<String> description) {
    return new Module(namespace, definitions, termDependencies, typeDependencies, description);
  }
}
