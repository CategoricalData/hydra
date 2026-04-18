// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class VariableDeclaratorId implements Serializable, Comparable<VariableDeclaratorId> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.VariableDeclaratorId");

  public static final hydra.core.Name IDENTIFIER = new hydra.core.Name("identifier");

  public static final hydra.core.Name DIMS = new hydra.core.Name("dims");

  public final hydra.java.syntax.Identifier identifier;

  public final hydra.util.Maybe<hydra.java.syntax.Dims> dims;

  public VariableDeclaratorId (hydra.java.syntax.Identifier identifier, hydra.util.Maybe<hydra.java.syntax.Dims> dims) {
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
    cmp = hydra.util.Comparing.compare(
      identifier,
      other.identifier);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      dims,
      other.dims);
  }

  public VariableDeclaratorId withIdentifier(hydra.java.syntax.Identifier identifier) {
    return new VariableDeclaratorId(identifier, dims);
  }

  public VariableDeclaratorId withDims(hydra.util.Maybe<hydra.java.syntax.Dims> dims) {
    return new VariableDeclaratorId(identifier, dims);
  }
}
