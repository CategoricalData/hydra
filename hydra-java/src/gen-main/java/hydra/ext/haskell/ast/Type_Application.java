package hydra.ext.haskell.ast;

public class Type_Application {
  public final Type context;
  
  public final Type argument;
  
  public Type_Application (Type context, Type argument) {
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
  
  public Type_Application withContext(Type context) {
    return new Type_Application(context, argument);
  }
  
  public Type_Application withArgument(Type argument) {
    return new Type_Application(context, argument);
  }
}