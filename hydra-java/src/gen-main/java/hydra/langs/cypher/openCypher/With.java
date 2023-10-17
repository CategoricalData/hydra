package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class With implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.With");
  
  public final hydra.langs.cypher.openCypher.ProjectionBody projection;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> where;
  
  public With (hydra.langs.cypher.openCypher.ProjectionBody projection, java.util.Optional<hydra.langs.cypher.openCypher.Expression> where) {
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
    return new With(projection, where);
  }
  
  public With withWhere(java.util.Optional<hydra.langs.cypher.openCypher.Expression> where) {
    return new With(projection, where);
  }
}