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
  public final java.util.List<hydra.core.Element<M>> elements;
  
  /**
   * Any additional modules this one has a direct dependency upon
   */
  public final java.util.List<hydra.graph.Module<M>> dependencies;
  
  public Module (hydra.graph.Namespace namespace, java.util.List<hydra.core.Element<M>> elements, java.util.List<hydra.graph.Module<M>> dependencies) {
    this.namespace = namespace;
    this.elements = elements;
    this.dependencies = dependencies;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Module)) {
      return false;
    }
    Module o = (Module) (other);
    return namespace.equals(o.namespace) && elements.equals(o.elements) && dependencies.equals(o.dependencies);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespace.hashCode() + 3 * elements.hashCode() + 5 * dependencies.hashCode();
  }
  
  public Module withNamespace(hydra.graph.Namespace namespace) {
    return new Module(namespace, elements, dependencies);
  }
  
  public Module withElements(java.util.List<hydra.core.Element<M>> elements) {
    return new Module(namespace, elements, dependencies);
  }
  
  public Module withDependencies(java.util.List<hydra.graph.Module<M>> dependencies) {
    return new Module(namespace, elements, dependencies);
  }
}