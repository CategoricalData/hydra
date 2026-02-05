// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * An abstract edge based on a record type
 */
public class Edge implements Serializable, Comparable<Edge> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.query.Edge");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_OUT = new hydra.core.Name("out");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  /**
   * The name of a record type, for which the edge also specifies an out- and an in- projection
   */
  public final hydra.core.Name type;
  
  /**
   * The field representing the out-projection of the edge. Defaults to 'out'.
   */
  public final hydra.util.Maybe<hydra.core.Name> out;
  
  /**
   * The field representing the in-projection of the edge. Defaults to 'in'.
   */
  public final hydra.util.Maybe<hydra.core.Name> in;
  
  public Edge (hydra.core.Name type, hydra.util.Maybe<hydra.core.Name> out, hydra.util.Maybe<hydra.core.Name> in) {
    this.type = type;
    this.out = out;
    this.in = in;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Edge)) {
      return false;
    }
    Edge o = (Edge) (other);
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.out,
      o.out) && java.util.Objects.equals(
      this.in,
      o.in);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(out) + 5 * java.util.Objects.hashCode(in);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Edge other) {
    int cmp = 0;
    cmp = ((Comparable) (type)).compareTo(other.type);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      out.hashCode(),
      other.out.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      in.hashCode(),
      other.in.hashCode());
  }
  
  public Edge withType(hydra.core.Name type) {
    return new Edge(type, out, in);
  }
  
  public Edge withOut(hydra.util.Maybe<hydra.core.Name> out) {
    return new Edge(type, out, in);
  }
  
  public Edge withIn(hydra.util.Maybe<hydra.core.Name> in) {
    return new Edge(type, out, in);
  }
}
