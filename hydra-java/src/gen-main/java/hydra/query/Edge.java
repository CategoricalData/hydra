// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import hydra.core.FieldName;
import hydra.util.Opt;

import java.io.Serializable;

/**
 * An abstract edge based on a record type
 */
public class Edge implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/query.Edge");
  
  /**
   * The name of a record type, for which the edge also specifies an out- and an in- projection
   */
  public final hydra.core.Name type;
  
  /**
   * The field representing the out-projection of the edge. Defaults to 'out'.
   */
  public final Opt<FieldName> out;
  
  /**
   * The field representing the in-projection of the edge. Defaults to 'in'.
   */
  public final Opt<FieldName> in;
  
  public Edge (hydra.core.Name type, Opt<FieldName> out, Opt<FieldName> in) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (out == null) {
      throw new IllegalArgumentException("null value for 'out' argument");
    }
    if (in == null) {
      throw new IllegalArgumentException("null value for 'in' argument");
    }
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
    return type.equals(o.type) && out.equals(o.out) && in.equals(o.in);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * out.hashCode() + 5 * in.hashCode();
  }
  
  public Edge withType(hydra.core.Name type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new Edge(type, out, in);
  }
  
  public Edge withOut(Opt<FieldName> out) {
    if (out == null) {
      throw new IllegalArgumentException("null value for 'out' argument");
    }
    return new Edge(type, out, in);
  }
  
  public Edge withIn(Opt<FieldName> in) {
    if (in == null) {
      throw new IllegalArgumentException("null value for 'in' argument");
    }
    return new Edge(type, out, in);
  }
}
