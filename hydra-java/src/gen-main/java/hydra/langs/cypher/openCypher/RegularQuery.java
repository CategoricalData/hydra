// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class RegularQuery implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RegularQuery");
  
  public final hydra.langs.cypher.openCypher.SingleQuery head;
  
  public final java.util.List<hydra.langs.cypher.openCypher.Union> rest;
  
  public RegularQuery (hydra.langs.cypher.openCypher.SingleQuery head, java.util.List<hydra.langs.cypher.openCypher.Union> rest) {
    if (head == null) {
      throw new IllegalArgumentException("null value for 'head' argument");
    }
    if (rest == null) {
      throw new IllegalArgumentException("null value for 'rest' argument");
    }
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
    if (head == null) {
      throw new IllegalArgumentException("null value for 'head' argument");
    }
    return new RegularQuery(head, rest);
  }
  
  public RegularQuery withRest(java.util.List<hydra.langs.cypher.openCypher.Union> rest) {
    if (rest == null) {
      throw new IllegalArgumentException("null value for 'rest' argument");
    }
    return new RegularQuery(head, rest);
  }
}