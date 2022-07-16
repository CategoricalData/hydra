package hydra.ext.java.syntax;

public class VariableDeclaratorId {
  public final Identifier identifier;
  
  public final java.util.Optional<Dims> dims;
  
  public VariableDeclaratorId (Identifier identifier, java.util.Optional<Dims> dims) {
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
  
  public VariableDeclaratorId withIdentifier(Identifier identifier) {
    return new VariableDeclaratorId(identifier, dims);
  }
  
  public VariableDeclaratorId withDims(java.util.Optional<Dims> dims) {
    return new VariableDeclaratorId(identifier, dims);
  }
}