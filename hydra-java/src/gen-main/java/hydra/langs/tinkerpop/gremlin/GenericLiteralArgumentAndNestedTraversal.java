// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralArgumentAndNestedTraversal implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.GenericLiteralArgumentAndNestedTraversal");
  
  public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument object;
  
  public final hydra.langs.tinkerpop.gremlin.NestedTraversal traversal;
  
  public GenericLiteralArgumentAndNestedTraversal (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument object, hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    if (traversal == null) {
      throw new IllegalArgumentException("null value for 'traversal' argument");
    }
    this.object = object;
    this.traversal = traversal;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralArgumentAndNestedTraversal)) {
      return false;
    }
    GenericLiteralArgumentAndNestedTraversal o = (GenericLiteralArgumentAndNestedTraversal) (other);
    return object.equals(o.object) && traversal.equals(o.traversal);
  }
  
  @Override
  public int hashCode() {
    return 2 * object.hashCode() + 3 * traversal.hashCode();
  }
  
  public GenericLiteralArgumentAndNestedTraversal withObject(hydra.langs.tinkerpop.gremlin.GenericLiteralArgument object) {
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    return new GenericLiteralArgumentAndNestedTraversal(object, traversal);
  }
  
  public GenericLiteralArgumentAndNestedTraversal withTraversal(hydra.langs.tinkerpop.gremlin.NestedTraversal traversal) {
    if (traversal == null) {
      throw new IllegalArgumentException("null value for 'traversal' argument");
    }
    return new GenericLiteralArgumentAndNestedTraversal(object, traversal);
  }
}