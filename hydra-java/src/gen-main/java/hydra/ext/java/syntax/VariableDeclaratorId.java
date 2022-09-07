package hydra.ext.java.syntax;

public class VariableDeclaratorId {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.VariableDeclaratorId");
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public final java.util.Optional<hydra.ext.java.syntax.Dims> dims;
  
  public VariableDeclaratorId (hydra.ext.java.syntax.Identifier identifier, java.util.Optional<hydra.ext.java.syntax.Dims> dims) {
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
  
  public VariableDeclaratorId withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    return new VariableDeclaratorId(identifier, dims);
  }
  
  public VariableDeclaratorId withDims(java.util.Optional<hydra.ext.java.syntax.Dims> dims) {
    return new VariableDeclaratorId(identifier, dims);
  }
}