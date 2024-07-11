// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RegularQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RegularQuery");
  
  public final hydra.langs.cypher.openCypher.SingleQuery head;
  
  public final java.util.List<hydra.langs.cypher.openCypher.Union> rest;
  
  public RegularQuery (hydra.langs.cypher.openCypher.SingleQuery head, java.util.List<hydra.langs.cypher.openCypher.Union> rest) {
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
  
  public RegularQuery withHead(hydra.langs.cypher.openCypher.SingleQuery head) {
    java.util.Objects.requireNonNull((head));
    return new RegularQuery(head, rest);
  }
  
  public RegularQuery withRest(java.util.List<hydra.langs.cypher.openCypher.Union> rest) {
    java.util.Objects.requireNonNull((rest));
    return new RegularQuery(head, rest);
  }
}