package hydra.ext.haskell.ast;

public class Type_Application {
  public final hydra.ext.haskell.ast.Type context;
  
  public final hydra.ext.haskell.ast.Type argument;
  
  public Type_Application (hydra.ext.haskell.ast.Type context, hydra.ext.haskell.ast.Type argument) {
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
    return new Type_Application(context, argument);
  }
  
  public Type_Application withArgument(hydra.ext.haskell.ast.Type argument) {
    return new Type_Application(context, argument);
  }
}