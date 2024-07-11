// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class With implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.With");
  
  public final hydra.langs.cypher.openCypher.ProjectionBody projection;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where;
  
  public With (hydra.langs.cypher.openCypher.ProjectionBody projection, hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    if (projection == null) {
      throw new IllegalArgumentException("null value for 'projection' argument");
    }
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
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
  
  public With withProjection(hydra.langs.cypher.openCypher.ProjectionBody projection) {
    if (projection == null) {
      throw new IllegalArgumentException("null value for 'projection' argument");
    }
    return new With(projection, where);
  }
  
  public With withWhere(hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    if (where == null) {
      throw new IllegalArgumentException("null value for 'where' argument");
    }
    return new With(projection, where);
  }
}