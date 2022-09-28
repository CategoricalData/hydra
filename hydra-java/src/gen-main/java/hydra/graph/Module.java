package hydra.graph;

/**
 * A logical collection of elements; a graph subset with dependencies on zero or more other subsets
 */
public class Module<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/graph.Module");
  
  /**
   * A common prefix for all element names in the module
   */
  public final hydra.graph.Namespace namespace;
  
  /**
   * The elements defined in this module
   */
  public final java.util.List<hydra.graph.Element<M>> elements;
  
  /**
   * Any additional modules this one has a direct dependency upon
   */
  public final java.util.List<hydra.graph.Module<M>> dependencies;
  
  /**
   * An optional human-readable description of the module
   */
  public final java.util.Optional<String> description;
  
  public Module (hydra.graph.Namespace namespace, java.util.List<hydra.graph.Element<M>> elements, java.util.List<hydra.graph.Module<M>> dependencies, java.util.Optional<String> description) {
    this.namespace = namespace;
    this.elements = elements;
    this.dependencies = dependencies;
    this.description = description;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Module)) {
      return false;
    }
    Module o = (Module) (other);
    return namespace.equals(o.namespace) && elements.equals(o.elements) && dependencies.equals(o.dependencies) && description.equals(o.description);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespace.hashCode() + 3 * elements.hashCode() + 5 * dependencies.hashCode() + 7 * description.hashCode();
  }
  
  public Module withNamespace(hydra.graph.Namespace namespace) {
    return new Module(namespace, elements, dependencies, description);
  }
  
  public Module withElements(java.util.List<hydra.graph.Element<M>> elements) {
    return new Module(namespace, elements, dependencies, description);
  }
  
  public Module withDependencies(java.util.List<hydra.graph.Module<M>> dependencies) {
    return new Module(namespace, elements, dependencies, description);
  }
  
  public Module withDescription(java.util.Optional<String> description) {
    return new Module(namespace, elements, dependencies, description);
  }
}