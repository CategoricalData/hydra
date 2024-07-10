// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.propertyGraph;

import java.io.Serializable;

/**
 * An element together with its dependencies in some context
 */
public class ElementTree<V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.ElementTree");
  
  public final hydra.langs.tinkerpop.propertyGraph.Element<V> self;
  
  public final java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTree<V>> dependencies;
  
  public ElementTree (hydra.langs.tinkerpop.propertyGraph.Element<V> self, java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTree<V>> dependencies) {
    if (self == null) {
      throw new IllegalArgumentException("null value for 'self' argument");
    }
    if (dependencies == null) {
      throw new IllegalArgumentException("null value for 'dependencies' argument");
    }
    this.self = self;
    this.dependencies = dependencies;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementTree)) {
      return false;
    }
    ElementTree o = (ElementTree) (other);
    return self.equals(o.self) && dependencies.equals(o.dependencies);
  }
  
  @Override
  public int hashCode() {
    return 2 * self.hashCode() + 3 * dependencies.hashCode();
  }
  
  public ElementTree withSelf(hydra.langs.tinkerpop.propertyGraph.Element<V> self) {
    if (self == null) {
      throw new IllegalArgumentException("null value for 'self' argument");
    }
    return new ElementTree(self, dependencies);
  }
  
  public ElementTree withDependencies(java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTree<V>> dependencies) {
    if (dependencies == null) {
      throw new IllegalArgumentException("null value for 'dependencies' argument");
    }
    return new ElementTree(self, dependencies);
  }
}