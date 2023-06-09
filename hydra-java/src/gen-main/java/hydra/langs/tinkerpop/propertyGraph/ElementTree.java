package hydra.langs.tinkerpop.propertyGraph;

/**
 * An element together with its dependencies in some context
 */
public class ElementTree<V, E, P> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.ElementTree");
  
  public final hydra.langs.tinkerpop.propertyGraph.Element<V, E, P> primary;
  
  public final java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTree<V, E, P>> dependencies;
  
  public ElementTree (hydra.langs.tinkerpop.propertyGraph.Element<V, E, P> primary, java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTree<V, E, P>> dependencies) {
    this.primary = primary;
    this.dependencies = dependencies;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementTree)) {
      return false;
    }
    ElementTree o = (ElementTree) (other);
    return primary.equals(o.primary) && dependencies.equals(o.dependencies);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * dependencies.hashCode();
  }
  
  public ElementTree withPrimary(hydra.langs.tinkerpop.propertyGraph.Element<V, E, P> primary) {
    return new ElementTree(primary, dependencies);
  }
  
  public ElementTree withDependencies(java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTree<V, E, P>> dependencies) {
    return new ElementTree(primary, dependencies);
  }
}