// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class VariableDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.VariableDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.VariableInitializer> initializer;
  
  public VariableDeclarator (hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.VariableInitializer> initializer) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((initializer));
    this.identifier = identifier;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariableDeclarator)) {
      return false;
    }
    VariableDeclarator o = (VariableDeclarator) (other);
    return identifier.equals(o.identifier) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * initializer.hashCode();
  }
  
  public VariableDeclarator withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new VariableDeclarator(identifier, initializer);
  }
  
  public VariableDeclarator withInitializer(hydra.util.Opt<hydra.ext.csharp.syntax.VariableInitializer> initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new VariableDeclarator(identifier, initializer);
  }
}