package hydra.langs.tinkerpop.propertyGraph;

/**
 * An element type together with its dependencies in some context
 */
public class ElementTypeTree<V, E, P> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.ElementTypeTree");
  
  public final hydra.langs.tinkerpop.propertyGraph.ElementType<V, E, P> primary;
  
  public final java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTypeTree<V, E, P>> dependencies;
  
  public ElementTypeTree (hydra.langs.tinkerpop.propertyGraph.ElementType<V, E, P> primary, java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTypeTree<V, E, P>> dependencies) {
    this.primary = primary;
    this.dependencies = dependencies;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementTypeTree)) {
      return false;
    }
    ElementTypeTree o = (ElementTypeTree) (other);
    return primary.equals(o.primary) && dependencies.equals(o.dependencies);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * dependencies.hashCode();
  }
  
  public ElementTypeTree withPrimary(hydra.langs.tinkerpop.propertyGraph.ElementType<V, E, P> primary) {
    return new ElementTypeTree(primary, dependencies);
  }
  
  public ElementTypeTree withDependencies(java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTypeTree<V, E, P>> dependencies) {
    return new ElementTypeTree(primary, dependencies);
  }
}