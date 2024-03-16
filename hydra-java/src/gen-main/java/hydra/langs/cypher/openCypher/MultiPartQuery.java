package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class MultiPartQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MultiPartQuery");
  
  public final java.util.List<hydra.langs.cypher.openCypher.WithClause> with;
  
  public final hydra.langs.cypher.openCypher.SinglePartQuery body;
  
  public MultiPartQuery (java.util.List<hydra.langs.cypher.openCypher.WithClause> with, hydra.langs.cypher.openCypher.SinglePartQuery body) {
    this.with = with;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiPartQuery)) {
      return false;
    }
    MultiPartQuery o = (MultiPartQuery) (other);
    return with.equals(o.with) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * with.hashCode() + 3 * body.hashCode();
  }
  
  public MultiPartQuery withWith(java.util.List<hydra.langs.cypher.openCypher.WithClause> with) {
    return new MultiPartQuery(with, body);
  }
  
  public MultiPartQuery withBody(hydra.langs.cypher.openCypher.SinglePartQuery body) {
    return new MultiPartQuery(with, body);
  }
}