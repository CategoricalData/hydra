// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public class RegularQuery implements Serializable, Comparable<RegularQuery> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.RegularQuery");
  
  public static final hydra.core.Name HEAD = new hydra.core.Name("head");
  
  public static final hydra.core.Name REST = new hydra.core.Name("rest");
  
  public final hydra.ext.cypher.openCypher.SingleQuery head;
  
  public final hydra.util.ConsList<hydra.ext.cypher.openCypher.Union> rest;
  
  public RegularQuery (hydra.ext.cypher.openCypher.SingleQuery head, hydra.util.ConsList<hydra.ext.cypher.openCypher.Union> rest) {
    this.head = head;
    this.rest = rest;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RegularQuery)) {
      return false;
    }
    RegularQuery o = (RegularQuery) other;
    return java.util.Objects.equals(
      this.head,
      o.head) && java.util.Objects.equals(
      this.rest,
      o.rest);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(head) + 3 * java.util.Objects.hashCode(rest);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RegularQuery other) {
    int cmp = 0;
    cmp = ((Comparable) head).compareTo(other.head);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      rest.hashCode(),
      other.rest.hashCode());
  }
  
  public RegularQuery withHead(hydra.ext.cypher.openCypher.SingleQuery head) {
    return new RegularQuery(head, rest);
  }
  
  public RegularQuery withRest(hydra.util.ConsList<hydra.ext.cypher.openCypher.Union> rest) {
    return new RegularQuery(head, rest);
  }
}
