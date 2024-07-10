// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class VariableDeclarator implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.VariableDeclarator");
  
  public final hydra.langs.java.syntax.VariableDeclaratorId id;
  
  public final java.util.Optional<hydra.langs.java.syntax.VariableInitializer> initializer;
  
  public VariableDeclarator (hydra.langs.java.syntax.VariableDeclaratorId id, java.util.Optional<hydra.langs.java.syntax.VariableInitializer> initializer) {
    if (id == null) {
      throw new IllegalArgumentException("null value for 'id' argument");
    }
    if (initializer == null) {
      throw new IllegalArgumentException("null value for 'initializer' argument");
    }
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
  
  public VariableDeclarator withId(hydra.langs.java.syntax.VariableDeclaratorId id) {
    if (id == null) {
      throw new IllegalArgumentException("null value for 'id' argument");
    }
    return new VariableDeclarator(id, initializer);
  }
  
  public VariableDeclarator withInitializer(java.util.Optional<hydra.langs.java.syntax.VariableInitializer> initializer) {
    if (initializer == null) {
      throw new IllegalArgumentException("null value for 'initializer' argument");
    }
    return new VariableDeclarator(id, initializer);
  }
}