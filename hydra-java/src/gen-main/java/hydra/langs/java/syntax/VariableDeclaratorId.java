// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class VariableDeclaratorId implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.VariableDeclaratorId");
  
  public final hydra.langs.java.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.Dims> dims;
  
  public VariableDeclaratorId (hydra.langs.java.syntax.Identifier identifier, hydra.util.Opt<hydra.langs.java.syntax.Dims> dims) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    if (dims == null) {
      throw new IllegalArgumentException("null value for 'dims' argument");
    }
    this.identifier = identifier;
    this.dims = dims;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableDeclaratorId)) {
      return false;
    }
    VariableDeclaratorId o = (VariableDeclaratorId) (other);
    return identifier.equals(o.identifier) && dims.equals(o.dims);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * dims.hashCode();
  }
  
  public VariableDeclaratorId withIdentifier(hydra.langs.java.syntax.Identifier identifier) {
    if (identifier == null) {
      throw new IllegalArgumentException("null value for 'identifier' argument");
    }
    return new VariableDeclaratorId(identifier, dims);
  }
  
  public VariableDeclaratorId withDims(hydra.util.Opt<hydra.langs.java.syntax.Dims> dims) {
    if (dims == null) {
      throw new IllegalArgumentException("null value for 'dims' argument");
    }
    return new VariableDeclaratorId(identifier, dims);
  }
}