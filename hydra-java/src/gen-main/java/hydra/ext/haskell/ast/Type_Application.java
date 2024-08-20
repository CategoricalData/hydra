// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class Type_Application implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.Type.Application");
  
  public static final hydra.core.Name FIELD_NAME_CONTEXT = new hydra.core.Name("context");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENT = new hydra.core.Name("argument");
  
  public final hydra.ext.haskell.ast.Type context;
  
  public final hydra.ext.haskell.ast.Type argument;
  
  public Type_Application (hydra.ext.haskell.ast.Type context, hydra.ext.haskell.ast.Type argument) {
    java.util.Objects.requireNonNull((context));
    java.util.Objects.requireNonNull((argument));
    this.context = context;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Application)) {
      return false;
    }
    Type_Application o = (Type_Application) (other);
    return context.equals(o.context) && argument.equals(o.argument);
  }
  
  @Override
  public int hashCode() {
    return 2 * context.hashCode() + 3 * argument.hashCode();
  }
  
  public Type_Application withContext(hydra.ext.haskell.ast.Type context) {
    java.util.Objects.requireNonNull((context));
    return new Type_Application(context, argument);
  }
  
  public Type_Application withArgument(hydra.ext.haskell.ast.Type argument) {
    java.util.Objects.requireNonNull((argument));
    return new Type_Application(context, argument);
  }
}
