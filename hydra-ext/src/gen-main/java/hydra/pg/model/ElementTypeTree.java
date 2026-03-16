// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * An element type together with its dependencies in some context
 */
public class ElementTypeTree<T> implements Serializable, Comparable<ElementTypeTree<T>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.ElementTypeTree");
  
  public static final hydra.core.Name SELF = new hydra.core.Name("self");
  
  public static final hydra.core.Name DEPENDENCIES = new hydra.core.Name("dependencies");
  
  public final hydra.pg.model.ElementType<T> self;
  
  public final hydra.util.ConsList<hydra.pg.model.ElementTypeTree<T>> dependencies;
  
  public ElementTypeTree (hydra.pg.model.ElementType<T> self, hydra.util.ConsList<hydra.pg.model.ElementTypeTree<T>> dependencies) {
    this.self = self;
    this.dependencies = dependencies;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ElementTypeTree)) {
      return false;
    }
    ElementTypeTree o = (ElementTypeTree) other;
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
  public int compareTo(ElementTypeTree other) {
    int cmp = 0;
    cmp = ((Comparable) self).compareTo(other.self);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) dependencies).compareTo(other.dependencies);
  }
  
  public ElementTypeTree withSelf(hydra.pg.model.ElementType<T> self) {
    return new ElementTypeTree(self, dependencies);
  }
  
  public ElementTypeTree withDependencies(hydra.util.ConsList<hydra.pg.model.ElementTypeTree<T>> dependencies) {
    return new ElementTypeTree(self, dependencies);
  }
}
