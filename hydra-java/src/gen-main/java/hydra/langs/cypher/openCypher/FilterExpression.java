package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class FilterExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.FilterExpression");
  
  public final hydra.langs.cypher.openCypher.IdInColl id;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> where;
  
  public FilterExpression (hydra.langs.cypher.openCypher.IdInColl id, java.util.Optional<hydra.langs.cypher.openCypher.Expression> where) {
    this.id = id;
    this.where = where;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FilterExpression)) {
      return false;
    }
    FilterExpression o = (FilterExpression) (other);
    return id.equals(o.id) && where.equals(o.where);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * where.hashCode();
  }
  
  public FilterExpression withId(hydra.langs.cypher.openCypher.IdInColl id) {
    return new FilterExpression(id, where);
  }
  
  public FilterExpression withWhere(java.util.Optional<hydra.langs.cypher.openCypher.Expression> where) {
    return new FilterExpression(id, where);
  }
}