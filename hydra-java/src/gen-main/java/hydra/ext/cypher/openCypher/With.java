// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class With implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.With");
  
  public static final hydra.core.Name FIELD_NAME_PROJECTION = new hydra.core.Name("projection");
  
  public static final hydra.core.Name FIELD_NAME_WHERE = new hydra.core.Name("where");
  
  public final hydra.ext.cypher.openCypher.ProjectionBody projection;
  
  public final hydra.util.Opt<hydra.ext.cypher.openCypher.Where> where;
  
  public With (hydra.ext.cypher.openCypher.ProjectionBody projection, hydra.util.Opt<hydra.ext.cypher.openCypher.Where> where) {
    java.util.Objects.requireNonNull((projection));
    java.util.Objects.requireNonNull((where));
    this.projection = projection;
    this.where = where;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof With)) {
      return false;
    }
    With o = (With) (other);
    return projection.equals(o.projection) && where.equals(o.where);
  }
  
  @Override
  public int hashCode() {
    return 2 * projection.hashCode() + 3 * where.hashCode();
  }
  
  public With withProjection(hydra.ext.cypher.openCypher.ProjectionBody projection) {
    java.util.Objects.requireNonNull((projection));
    return new With(projection, where);
  }
  
  public With withWhere(hydra.util.Opt<hydra.ext.cypher.openCypher.Where> where) {
    java.util.Objects.requireNonNull((where));
    return new With(projection, where);
  }
}
