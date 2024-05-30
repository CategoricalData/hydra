package hydra.langs.kusto.kql;

import java.io.Serializable;

public class Query implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Query");
  
  public final hydra.langs.kusto.kql.TabularExpression value;
  
  public Query (hydra.langs.kusto.kql.TabularExpression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Query)) {
      return false;
    }
    Query o = (Query) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}