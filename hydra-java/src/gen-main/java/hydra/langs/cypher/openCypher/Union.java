// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Union implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Union");
  
  public final Boolean all;
  
  public final hydra.langs.cypher.openCypher.SingleQuery query;
  
  public Union (Boolean all, hydra.langs.cypher.openCypher.SingleQuery query) {
    if (all == null) {
      throw new IllegalArgumentException("null value for 'all' argument");
    }
    if (query == null) {
      throw new IllegalArgumentException("null value for 'query' argument");
    }
    this.all = all;
    this.query = query;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Union)) {
      return false;
    }
    Union o = (Union) (other);
    return all.equals(o.all) && query.equals(o.query);
  }
  
  @Override
  public int hashCode() {
    return 2 * all.hashCode() + 3 * query.hashCode();
  }
  
  public Union withAll(Boolean all) {
    if (all == null) {
      throw new IllegalArgumentException("null value for 'all' argument");
    }
    return new Union(all, query);
  }
  
  public Union withQuery(hydra.langs.cypher.openCypher.SingleQuery query) {
    if (query == null) {
      throw new IllegalArgumentException("null value for 'query' argument");
    }
    return new Union(all, query);
  }
}