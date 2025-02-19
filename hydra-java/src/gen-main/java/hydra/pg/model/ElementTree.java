// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * An element together with its dependencies in some context
 */
public class ElementTree<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.model.ElementTree");
  
  public static final hydra.core.Name FIELD_NAME_SELF = new hydra.core.Name("self");
  
  public static final hydra.core.Name FIELD_NAME_DEPENDENCIES = new hydra.core.Name("dependencies");
  
  public final hydra.pg.model.Element<V> self;
  
  public final java.util.List<hydra.pg.model.ElementTree<V>> dependencies;
  
  public ElementTree (hydra.pg.model.Element<V> self, java.util.List<hydra.pg.model.ElementTree<V>> dependencies) {
    java.util.Objects.requireNonNull((self));
    java.util.Objects.requireNonNull((dependencies));
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
  
  public ElementTree withSelf(hydra.pg.model.Element<V> self) {
    java.util.Objects.requireNonNull((self));
    return new ElementTree(self, dependencies);
  }
  
  public ElementTree withDependencies(java.util.List<hydra.pg.model.ElementTree<V>> dependencies) {
    java.util.Objects.requireNonNull((dependencies));
    return new ElementTree(self, dependencies);
  }
}