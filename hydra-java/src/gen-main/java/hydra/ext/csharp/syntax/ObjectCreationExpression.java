// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ObjectCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ObjectCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public static final hydra.core.Name FIELD_NAME_INITIALIZER = new hydra.core.Name("initializer");
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> arguments;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ObjectOrCollectionInitializer> initializer;
  
  public ObjectCreationExpression (hydra.ext.csharp.syntax.Type type, hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> arguments, hydra.util.Opt<hydra.ext.csharp.syntax.ObjectOrCollectionInitializer> initializer) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((arguments));
    java.util.Objects.requireNonNull((initializer));
    this.type = type;
    this.arguments = arguments;
    this.initializer = initializer;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ObjectCreationExpression)) {
      return false;
    }
    ObjectCreationExpression o = (ObjectCreationExpression) (other);
    return type.equals(o.type) && arguments.equals(o.arguments) && initializer.equals(o.initializer);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * arguments.hashCode() + 5 * initializer.hashCode();
  }
  
  public ObjectCreationExpression withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new ObjectCreationExpression(type, arguments, initializer);
  }
  
  public ObjectCreationExpression withArguments(hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new ObjectCreationExpression(type, arguments, initializer);
  }
  
  public ObjectCreationExpression withInitializer(hydra.util.Opt<hydra.ext.csharp.syntax.ObjectOrCollectionInitializer> initializer) {
    java.util.Objects.requireNonNull((initializer));
    return new ObjectCreationExpression(type, arguments, initializer);
  }
}