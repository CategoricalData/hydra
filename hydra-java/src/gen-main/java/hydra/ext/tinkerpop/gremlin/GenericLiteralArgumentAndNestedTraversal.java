// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralArgumentAndNestedTraversal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.GenericLiteralArgumentAndNestedTraversal");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL = new hydra.core.Name("traversal");
  
  public final hydra.ext.tinkerpop.gremlin.GenericLiteralArgument object;
  
  public final hydra.ext.tinkerpop.gremlin.NestedTraversal traversal;
  
  public GenericLiteralArgumentAndNestedTraversal (hydra.ext.tinkerpop.gremlin.GenericLiteralArgument object, hydra.ext.tinkerpop.gremlin.NestedTraversal traversal) {
    java.util.Objects.requireNonNull((object));
    java.util.Objects.requireNonNull((traversal));
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
  
  public GenericLiteralArgumentAndNestedTraversal withObject(hydra.ext.tinkerpop.gremlin.GenericLiteralArgument object) {
    java.util.Objects.requireNonNull((object));
    return new GenericLiteralArgumentAndNestedTraversal(object, traversal);
  }
  
  public GenericLiteralArgumentAndNestedTraversal withTraversal(hydra.ext.tinkerpop.gremlin.NestedTraversal traversal) {
    java.util.Objects.requireNonNull((traversal));
    return new GenericLiteralArgumentAndNestedTraversal(object, traversal);
  }
}
