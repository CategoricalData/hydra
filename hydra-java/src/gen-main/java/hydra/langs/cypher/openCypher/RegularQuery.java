package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RegularQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RegularQuery");
  
  public final hydra.langs.cypher.openCypher.SingleQuery query;
  
  public final java.util.List<hydra.langs.cypher.openCypher.Union> union;
  
  public RegularQuery (hydra.langs.cypher.openCypher.SingleQuery query, java.util.List<hydra.langs.cypher.openCypher.Union> union) {
    this.query = query;
    this.union = union;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RegularQuery)) {
      return false;
    }
    RegularQuery o = (RegularQuery) (other);
    return query.equals(o.query) && union.equals(o.union);
  }
  
  @Override
  public int hashCode() {
    return 2 * query.hashCode() + 3 * union.hashCode();
  }
  
  public RegularQuery withQuery(hydra.langs.cypher.openCypher.SingleQuery query) {
    return new RegularQuery(query, union);
  }
  
  public RegularQuery withUnion(java.util.List<hydra.langs.cypher.openCypher.Union> union) {
    return new RegularQuery(query, union);
  }
}