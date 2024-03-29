package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class FilterExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.FilterExpression");
  
  public final hydra.langs.cypher.openCypher.IdInColl idInColl;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.Where> where;
  
  public FilterExpression (hydra.langs.cypher.openCypher.IdInColl idInColl, java.util.Optional<hydra.langs.cypher.openCypher.Where> where) {
    this.idInColl = idInColl;
    this.where = where;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FilterExpression)) {
      return false;
    }
    FilterExpression o = (FilterExpression) (other);
    return idInColl.equals(o.idInColl) && where.equals(o.where);
  }
  
  @Override
  public int hashCode() {
    return 2 * idInColl.hashCode() + 3 * where.hashCode();
  }
  
  public FilterExpression withIdInColl(hydra.langs.cypher.openCypher.IdInColl idInColl) {
    return new FilterExpression(idInColl, where);
  }
  
  public FilterExpression withWhere(java.util.Optional<hydra.langs.cypher.openCypher.Where> where) {
    return new FilterExpression(idInColl, where);
  }
}