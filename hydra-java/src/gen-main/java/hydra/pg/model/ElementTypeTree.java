// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * An element type together with its dependencies in some context
 */
public class ElementTypeTree<T> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/pg/model.ElementTypeTree");
  
  public static final hydra.core.Name FIELD_NAME_SELF = new hydra.core.Name("self");
  
  public static final hydra.core.Name FIELD_NAME_DEPENDENCIES = new hydra.core.Name("dependencies");
  
  public final hydra.pg.model.ElementType<T> self;
  
  public final java.util.List<hydra.pg.model.ElementTypeTree<T>> dependencies;
  
  public ElementTypeTree (hydra.pg.model.ElementType<T> self, java.util.List<hydra.pg.model.ElementTypeTree<T>> dependencies) {
    java.util.Objects.requireNonNull((self));
    java.util.Objects.requireNonNull((dependencies));
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
  
  public ElementTypeTree withSelf(hydra.pg.model.ElementType<T> self) {
    java.util.Objects.requireNonNull((self));
    return new ElementTypeTree(self, dependencies);
  }
  
  public ElementTypeTree withDependencies(java.util.List<hydra.pg.model.ElementTypeTree<T>> dependencies) {
    java.util.Objects.requireNonNull((dependencies));
    return new ElementTypeTree(self, dependencies);
  }
}