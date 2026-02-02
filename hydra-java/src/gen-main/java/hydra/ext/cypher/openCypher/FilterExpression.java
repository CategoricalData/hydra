// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class FilterExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.FilterExpression");
  
  public static final hydra.core.Name FIELD_NAME_ID_IN_COLL = new hydra.core.Name("idInColl");
  
  public static final hydra.core.Name FIELD_NAME_WHERE = new hydra.core.Name("where");
  
  public final hydra.ext.cypher.openCypher.IdInColl idInColl;
  
  public final hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where;
  
  public FilterExpression (hydra.ext.cypher.openCypher.IdInColl idInColl, hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where) {
    java.util.Objects.requireNonNull((idInColl));
    java.util.Objects.requireNonNull((where));
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
  
  public FilterExpression withIdInColl(hydra.ext.cypher.openCypher.IdInColl idInColl) {
    java.util.Objects.requireNonNull((idInColl));
    return new FilterExpression(idInColl, where);
  }
  
  public FilterExpression withWhere(hydra.util.Maybe<hydra.ext.cypher.openCypher.Where> where) {
    java.util.Objects.requireNonNull((where));
    return new FilterExpression(idInColl, where);
  }
}
