// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class FilterExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.FilterExpression");
  
  public static final hydra.core.Name FIELD_NAME_ID_IN_COLL = new hydra.core.Name("idInColl");
  
  public static final hydra.core.Name FIELD_NAME_WHERE = new hydra.core.Name("where");
  
  public final hydra.langs.cypher.openCypher.IdInColl idInColl;
  
  public final hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where;
  
  public FilterExpression (hydra.langs.cypher.openCypher.IdInColl idInColl, hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
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
  
  public FilterExpression withIdInColl(hydra.langs.cypher.openCypher.IdInColl idInColl) {
    java.util.Objects.requireNonNull((idInColl));
    return new FilterExpression(idInColl, where);
  }
  
  public FilterExpression withWhere(hydra.util.Opt<hydra.langs.cypher.openCypher.Where> where) {
    java.util.Objects.requireNonNull((where));
    return new FilterExpression(idInColl, where);
  }
}