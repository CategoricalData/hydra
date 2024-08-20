// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class VariableDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.VariableDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.java.syntax.VariableDeclaratorId id;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.VariableInitializer> initializer;
  
  public VariableDeclarator (hydra.ext.java.syntax.VariableDeclaratorId id, hydra.util.Opt<hydra.ext.java.syntax.VariableInitializer> initializer) {
    java.util.Objects.requireNonNull((id));
    java.util.Objects.requireNonNull((initializer));
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
  
  public VariableDeclarator withId(hydra.ext.java.syntax.VariableDeclaratorId id) {
    java.util.Objects.requireNonNull((id));
    return new VariableDeclarator(id, initializer);
  }
  
  public VariableDeclarator withInitializer(hydra.util.Opt<hydra.ext.java.syntax.VariableInitializer> initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new VariableDeclarator(id, initializer);
  }
}
