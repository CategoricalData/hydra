// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class VariableDeclaratorId implements Serializable, Comparable<VariableDeclaratorId> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.VariableDeclaratorId");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_DIMS = new hydra.core.Name("dims");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.Dims> dims;
  
  public VariableDeclaratorId (hydra.ext.java.syntax.Identifier identifier, hydra.util.Maybe<hydra.ext.java.syntax.Dims> dims) {
    this.identifier = identifier;
    this.dims = dims;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableDeclaratorId)) {
      return false;
    }
    VariableDeclaratorId o = (VariableDeclaratorId) other;
    return java.util.Objects.equals(
      this.identifier,
      o.identifier) && java.util.Objects.equals(
      this.dims,
      o.dims);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(identifier) + 3 * java.util.Objects.hashCode(dims);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VariableDeclaratorId other) {
    int cmp = 0;
    cmp = ((Comparable) identifier).compareTo(other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      dims.hashCode(),
      other.dims.hashCode());
  }
  
  public VariableDeclaratorId withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new VariableDeclaratorId(identifier, dims);
  }
  
  public VariableDeclaratorId withDims(hydra.util.Maybe<hydra.ext.java.syntax.Dims> dims) {
    return new VariableDeclaratorId(identifier, dims);
  }
}
