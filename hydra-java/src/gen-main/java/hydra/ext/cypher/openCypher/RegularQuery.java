// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RegularQuery implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/cypher/openCypher.RegularQuery");
  
  public static final hydra.core.Name FIELD_NAME_HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name FIELD_NAME_REST = new hydra.core.Name("rest");
  
  public final hydra.ext.cypher.openCypher.SingleQuery head;
  
  public final java.util.List<hydra.ext.cypher.openCypher.Union> rest;
  
  public RegularQuery (hydra.ext.cypher.openCypher.SingleQuery head, java.util.List<hydra.ext.cypher.openCypher.Union> rest) {
    java.util.Objects.requireNonNull((head));
    java.util.Objects.requireNonNull((rest));
    this.head = head;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RegularQuery)) {
      return false;
    }
    RegularQuery o = (RegularQuery) (other);
    return head.equals(o.head) && rest.equals(o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * head.hashCode() + 3 * rest.hashCode();
  }
  
  public RegularQuery withHead(hydra.ext.cypher.openCypher.SingleQuery head) {
    java.util.Objects.requireNonNull((head));
    return new RegularQuery(head, rest);
  }
  
  public RegularQuery withRest(java.util.List<hydra.ext.cypher.openCypher.Union> rest) {
    java.util.Objects.requireNonNull((rest));
    return new RegularQuery(head, rest);
  }
}
