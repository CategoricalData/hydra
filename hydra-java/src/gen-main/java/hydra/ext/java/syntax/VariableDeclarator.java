// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class VariableDeclarator implements Serializable, Comparable<VariableDeclarator> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.VariableDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.java.syntax.VariableDeclaratorId id;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer> initializer;
  
  public VariableDeclarator (hydra.ext.java.syntax.VariableDeclaratorId id, hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer> initializer) {
    this.id = id;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableDeclarator)) {
      return false;
    }
    VariableDeclarator o = (VariableDeclarator) other;
    return java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.initializer,
      o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(id) + 3 * java.util.Objects.hashCode(initializer);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(VariableDeclarator other) {
    int cmp = 0;
    cmp = ((Comparable) id).compareTo(other.id);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      initializer.hashCode(),
      other.initializer.hashCode());
  }
  
  public VariableDeclarator withId(hydra.ext.java.syntax.VariableDeclaratorId id) {
    return new VariableDeclarator(id, initializer);
  }
  
  public VariableDeclarator withInitializer(hydra.util.Maybe<hydra.ext.java.syntax.VariableInitializer> initializer) {
    return new VariableDeclarator(id, initializer);
  }
}
