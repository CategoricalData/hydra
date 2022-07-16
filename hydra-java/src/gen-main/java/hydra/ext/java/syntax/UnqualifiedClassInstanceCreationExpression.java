package hydra.ext.java.syntax;

public class UnqualifiedClassInstanceCreationExpression {
  public final java.util.List<TypeArgument> typeArguments;
  
  public final ClassOrInterfaceTypeToInstantiate classOrInterface;
  
  public final java.util.List<Expression> arguments;
  
  public final java.util.Optional<ClassBody> body;
  
  public UnqualifiedClassInstanceCreationExpression (java.util.List<TypeArgument> typeArguments, ClassOrInterfaceTypeToInstantiate classOrInterface, java.util.List<Expression> arguments, java.util.Optional<ClassBody> body) {
    this.typeArguments = typeArguments;
    this.classOrInterface = classOrInterface;
    this.arguments = arguments;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnqualifiedClassInstanceCreationExpression)) {
      return false;
    }
    UnqualifiedClassInstanceCreationExpression o = (UnqualifiedClassInstanceCreationExpression) (other);
    return typeArguments.equals(o.typeArguments) && classOrInterface.equals(o.classOrInterface) && arguments.equals(o.arguments) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeArguments.hashCode() + 3 * classOrInterface.hashCode() + 5 * arguments.hashCode() + 7 * body.hashCode();
  }
  
  public UnqualifiedClassInstanceCreationExpression withTypeArguments(java.util.List<TypeArgument> typeArguments) {
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withClassOrInterface(ClassOrInterfaceTypeToInstantiate classOrInterface) {
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withArguments(java.util.List<Expression> arguments) {
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withBody(java.util.Optional<ClassBody> body) {
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
}