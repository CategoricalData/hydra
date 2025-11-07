// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * An abstract edge based on a record type
 */
public class Edge implements Serializable {
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
  public final hydra.util.Opt<hydra.core.Name> out;
  
  /**
   * The field representing the in-projection of the edge. Defaults to 'in'.
   */
  public final hydra.util.Opt<hydra.core.Name> in;
  
  public Edge (hydra.core.Name type, hydra.util.Opt<hydra.core.Name> out, hydra.util.Opt<hydra.core.Name> in) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((out));
    java.util.Objects.requireNonNull((in));
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
    java.util.Objects.requireNonNull((type));
    return new Edge(type, out, in);
  }
  
  public Edge withOut(hydra.util.Opt<hydra.core.Name> out) {
    java.util.Objects.requireNonNull((out));
    return new Edge(type, out, in);
  }
  
  public Edge withIn(hydra.util.Opt<hydra.core.Name> in) {
    java.util.Objects.requireNonNull((in));
    return new Edge(type, out, in);
  }
}
