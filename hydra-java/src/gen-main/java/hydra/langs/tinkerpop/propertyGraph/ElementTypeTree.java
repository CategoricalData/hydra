// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.propertyGraph;

import java.io.Serializable;

/**
 * An element type together with its dependencies in some context
 */
public class ElementTypeTree<T> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.ElementTypeTree");
  
  public final hydra.langs.tinkerpop.propertyGraph.ElementType<T> self;
  
  public final java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTypeTree<T>> dependencies;
  
  public ElementTypeTree (hydra.langs.tinkerpop.propertyGraph.ElementType<T> self, java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTypeTree<T>> dependencies) {
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
    if (!(other instanceof ElementTypeTree)) {
      return false;
    }
    ElementTypeTree o = (ElementTypeTree) (other);
    return self.equals(o.self) && dependencies.equals(o.dependencies);
  }
  
  @Override
  public int hashCode() {
    return 2 * self.hashCode() + 3 * dependencies.hashCode();
  }
  
  public ElementTypeTree withSelf(hydra.langs.tinkerpop.propertyGraph.ElementType<T> self) {
    if (self == null) {
      throw new IllegalArgumentException("null value for 'self' argument");
    }
    return new ElementTypeTree(self, dependencies);
  }
  
  public ElementTypeTree withDependencies(java.util.List<hydra.langs.tinkerpop.propertyGraph.ElementTypeTree<T>> dependencies) {
    if (dependencies == null) {
      throw new IllegalArgumentException("null value for 'dependencies' argument");
    }
    return new ElementTypeTree(self, dependencies);
  }
}