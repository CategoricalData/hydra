package hydra.ext.java.syntax;

public class MethodReference_New {
  public final ClassType classType;
  
  public final java.util.List<TypeArgument> typeArguments;
  
  public MethodReference_New (ClassType classType, java.util.List<TypeArgument> typeArguments) {
    this.classType = classType;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_New)) {
      return false;
    }
    MethodReference_New o = (MethodReference_New) (other);
    return classType.equals(o.classType) && typeArguments.equals(o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * classType.hashCode() + 3 * typeArguments.hashCode();
  }
  
  public MethodReference_New withClassType(ClassType classType) {
    return new MethodReference_New(classType, typeArguments);
  }
  
  public MethodReference_New withTypeArguments(java.util.List<TypeArgument> typeArguments) {
    return new MethodReference_New(classType, typeArguments);
  }
}