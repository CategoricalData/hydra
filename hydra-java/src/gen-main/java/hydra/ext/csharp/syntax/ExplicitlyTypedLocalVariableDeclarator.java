// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ExplicitlyTypedLocalVariableDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ExplicitlyTypedLocalVariableDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.LocalVariableInitializer> initializer;
  
  public ExplicitlyTypedLocalVariableDeclarator (hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.LocalVariableInitializer> initializer) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((initializer));
    this.identifier = identifier;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExplicitlyTypedLocalVariableDeclarator)) {
      return false;
    }
    ExplicitlyTypedLocalVariableDeclarator o = (ExplicitlyTypedLocalVariableDeclarator) (other);
    return identifier.equals(o.identifier) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * initializer.hashCode();
  }
  
  public ExplicitlyTypedLocalVariableDeclarator withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new ExplicitlyTypedLocalVariableDeclarator(identifier, initializer);
  }
  
  public ExplicitlyTypedLocalVariableDeclarator withInitializer(hydra.util.Opt<hydra.ext.csharp.syntax.LocalVariableInitializer> initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new ExplicitlyTypedLocalVariableDeclarator(identifier, initializer);
  }
}