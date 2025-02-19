// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class MultiPartQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.MultiPartQuery");
  
  public static final hydra.core.Name FIELD_NAME_WITH = new hydra.core.Name("with");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.cypher.openCypher.WithClause> with;
  
  public final hydra.ext.cypher.openCypher.SinglePartQuery body;
  
  public MultiPartQuery (java.util.List<hydra.ext.cypher.openCypher.WithClause> with, hydra.ext.cypher.openCypher.SinglePartQuery body) {
    java.util.Objects.requireNonNull((with));
    java.util.Objects.requireNonNull((body));
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
  
  public MultiPartQuery withWith(java.util.List<hydra.ext.cypher.openCypher.WithClause> with) {
    java.util.Objects.requireNonNull((with));
    return new MultiPartQuery(with, body);
  }
  
  public MultiPartQuery withBody(hydra.ext.cypher.openCypher.SinglePartQuery body) {
    java.util.Objects.requireNonNull((body));
    return new MultiPartQuery(with, body);
  }
}