package hydra.ext.java.syntax;

public class VariableDeclarator {
  public final VariableDeclaratorId id;
  
  public final java.util.Optional<VariableInitializer> initializer;
  
  public VariableDeclarator (VariableDeclaratorId id, java.util.Optional<VariableInitializer> initializer) {
    this.id = id;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableDeclarator)) {
      return false;
    }
    VariableDeclarator o = (VariableDeclarator) (other);
    return id.equals(o.id) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * id.hashCode() + 3 * initializer.hashCode();
  }
  
  public VariableDeclarator withId(VariableDeclaratorId id) {
    return new VariableDeclarator(id, initializer);
  }
  
  public VariableDeclarator withInitializer(java.util.Optional<VariableInitializer> initializer) {
    return new VariableDeclarator(id, initializer);
  }
}