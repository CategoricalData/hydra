// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ConstructorDeclarator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ConstructorDeclarator");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETERS = new hydra.core.Name("parameters");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ConstructorInitializer> initializer;
  
  public ConstructorDeclarator (hydra.ext.csharp.syntax.Identifier name, hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters, hydra.util.Opt<hydra.ext.csharp.syntax.ConstructorInitializer> initializer) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((parameters));
    java.util.Objects.requireNonNull((initializer));
    this.name = name;
    this.parameters = parameters;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstructorDeclarator)) {
      return false;
    }
    ConstructorDeclarator o = (ConstructorDeclarator) (other);
    return name.equals(o.name) && parameters.equals(o.parameters) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * parameters.hashCode() + 5 * initializer.hashCode();
  }
  
  public ConstructorDeclarator withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new ConstructorDeclarator(name, parameters, initializer);
  }
  
  public ConstructorDeclarator withParameters(hydra.util.Opt<hydra.ext.csharp.syntax.FormalParameterList> parameters) {
    java.util.Objects.requireNonNull((parameters));
    return new ConstructorDeclarator(name, parameters, initializer);
  }
  
  public ConstructorDeclarator withInitializer(hydra.util.Opt<hydra.ext.csharp.syntax.ConstructorInitializer> initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new ConstructorDeclarator(name, parameters, initializer);
  }
}