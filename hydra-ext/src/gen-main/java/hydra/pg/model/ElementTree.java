// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * An element together with its dependencies in some context
 */
public class ElementTree<V> implements Serializable, Comparable<ElementTree<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.ElementTree");
  
  public static final hydra.core.Name SELF = new hydra.core.Name("self");
  
  public static final hydra.core.Name DEPENDENCIES = new hydra.core.Name("dependencies");
  
  public final hydra.pg.model.Element<V> self;
  
  public final hydra.util.ConsList<hydra.pg.model.ElementTree<V>> dependencies;
  
  public ElementTree (hydra.pg.model.Element<V> self, hydra.util.ConsList<hydra.pg.model.ElementTree<V>> dependencies) {
    this.self = self;
    this.dependencies = dependencies;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementTree)) {
      return false;
    }
    ElementTree o = (ElementTree) other;
    return java.util.Objects.equals(
      this.self,
      o.self) && java.util.Objects.equals(
      this.dependencies,
      o.dependencies);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(self) + 3 * java.util.Objects.hashCode(dependencies);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ElementTree other) {
    int cmp = 0;
    cmp = ((Comparable) self).compareTo(other.self);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      dependencies.hashCode(),
      other.dependencies.hashCode());
  }
  
  public ElementTree withSelf(hydra.pg.model.Element<V> self) {
    return new ElementTree(self, dependencies);
  }
  
  public ElementTree withDependencies(hydra.util.ConsList<hydra.pg.model.ElementTree<V>> dependencies) {
    return new ElementTree(self, dependencies);
  }
}
