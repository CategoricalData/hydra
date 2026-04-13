// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class GenericLiteralArgumentAndNestedTraversal implements Serializable, Comparable<GenericLiteralArgumentAndNestedTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public static final hydra.core.Name TRAVERSAL = new hydra.core.Name("traversal");

  public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument object;

  public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal;

  public GenericLiteralArgumentAndNestedTraversal (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument object, hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
    this.object = object;
    this.traversal = traversal;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GenericLiteralArgumentAndNestedTraversal)) {
      return false;
    }
    GenericLiteralArgumentAndNestedTraversal o = (GenericLiteralArgumentAndNestedTraversal) other;
    return java.util.Objects.equals(
      this.object,
      o.object) && java.util.Objects.equals(
      this.traversal,
      o.traversal);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(object) + 3 * java.util.Objects.hashCode(traversal);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(GenericLiteralArgumentAndNestedTraversal other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      object,
      other.object);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      traversal,
      other.traversal);
  }

  public GenericLiteralArgumentAndNestedTraversal withObject(hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument object) {
    return new GenericLiteralArgumentAndNestedTraversal(object, traversal);
  }

  public GenericLiteralArgumentAndNestedTraversal withTraversal(hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal traversal) {
    return new GenericLiteralArgumentAndNestedTraversal(object, traversal);
  }
}
