// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class Union implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Union");
  
  public final Boolean all;
  
  public final hydra.langs.cypher.openCypher.SingleQuery query;
  
  public Union (Boolean all, hydra.langs.cypher.openCypher.SingleQuery query) {
    java.util.Objects.requireNonNull((all));
    java.util.Objects.requireNonNull((query));
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
    java.util.Objects.requireNonNull((all));
    return new Union(all, query);
  }
  
  public Union withQuery(hydra.langs.cypher.openCypher.SingleQuery query) {
    java.util.Objects.requireNonNull((query));
    return new Union(all, query);
  }
}