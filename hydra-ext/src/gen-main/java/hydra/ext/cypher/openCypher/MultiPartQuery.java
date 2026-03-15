// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class MultiPartQuery implements Serializable, Comparable<MultiPartQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.MultiPartQuery");
  
  public static final hydra.core.Name WITH = new hydra.core.Name("with");
  
  public static final hydra.core.Name BODY = new hydra.core.Name("body");
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.WithClause> with;
  
  public final hydra.ext.cypher.openCypher.SinglePartQuery body;
  
  public MultiPartQuery (hydra.util.ConsList<hydra.ext.cypher.openCypher.WithClause> with, hydra.ext.cypher.openCypher.SinglePartQuery body) {
    this.with = with;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MultiPartQuery)) {
      return false;
    }
    MultiPartQuery o = (MultiPartQuery) other;
    return java.util.Objects.equals(
      this.with,
      o.with) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(with) + 3 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MultiPartQuery other) {
    int cmp = 0;
    cmp = Integer.compare(
      with.hashCode(),
      other.with.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }
  
  public MultiPartQuery withWith(hydra.util.ConsList<hydra.ext.cypher.openCypher.WithClause> with) {
    return new MultiPartQuery(with, body);
  }
  
  public MultiPartQuery withBody(hydra.ext.cypher.openCypher.SinglePartQuery body) {
    return new MultiPartQuery(with, body);
  }
}
