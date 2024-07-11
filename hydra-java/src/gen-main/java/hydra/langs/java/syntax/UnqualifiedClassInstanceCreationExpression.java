// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class UnqualifiedClassInstanceCreationExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/java/syntax.UnqualifiedClassInstanceCreationExpression");
  
  public final java.util.List<hydra.langs.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.langs.java.syntax.ClassOrInterfaceTypeToInstantiate classOrInterface;
  
  public final java.util.List<hydra.langs.java.syntax.Expression> arguments;
  
  public final hydra.util.Opt<hydra.langs.java.syntax.ClassBody> body;
  
  public UnqualifiedClassInstanceCreationExpression (java.util.List<hydra.langs.java.syntax.TypeArgument> typeArguments, hydra.langs.java.syntax.ClassOrInterfaceTypeToInstantiate classOrInterface, java.util.List<hydra.langs.java.syntax.Expression> arguments, hydra.util.Opt<hydra.langs.java.syntax.ClassBody> body) {
    if (typeArguments == null) {
      throw new IllegalArgumentException("null value for 'typeArguments' argument");
    }
    if (classOrInterface == null) {
      throw new IllegalArgumentException("null value for 'classOrInterface' argument");
    }
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
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
  
  public UnqualifiedClassInstanceCreationExpression withTypeArguments(java.util.List<hydra.langs.java.syntax.TypeArgument> typeArguments) {
    if (typeArguments == null) {
      throw new IllegalArgumentException("null value for 'typeArguments' argument");
    }
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withClassOrInterface(hydra.langs.java.syntax.ClassOrInterfaceTypeToInstantiate classOrInterface) {
    if (classOrInterface == null) {
      throw new IllegalArgumentException("null value for 'classOrInterface' argument");
    }
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withArguments(java.util.List<hydra.langs.java.syntax.Expression> arguments) {
    if (arguments == null) {
      throw new IllegalArgumentException("null value for 'arguments' argument");
    }
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withBody(hydra.util.Opt<hydra.langs.java.syntax.ClassBody> body) {
    if (body == null) {
      throw new IllegalArgumentException("null value for 'body' argument");
    }
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
}