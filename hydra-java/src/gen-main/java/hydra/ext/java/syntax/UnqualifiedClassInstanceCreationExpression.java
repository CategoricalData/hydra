// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class UnqualifiedClassInstanceCreationExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/java/syntax.UnqualifiedClassInstanceCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_OR_INTERFACE = new hydra.core.Name("classOrInterface");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate classOrInterface;
  
  public final java.util.List<hydra.ext.java.syntax.Expression> arguments;
  
  public final hydra.util.Opt<hydra.ext.java.syntax.ClassBody> body;
  
  public UnqualifiedClassInstanceCreationExpression (java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate classOrInterface, java.util.List<hydra.ext.java.syntax.Expression> arguments, hydra.util.Opt<hydra.ext.java.syntax.ClassBody> body) {
    java.util.Objects.requireNonNull((typeArguments));
    java.util.Objects.requireNonNull((classOrInterface));
    java.util.Objects.requireNonNull((arguments));
    java.util.Objects.requireNonNull((body));
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
  
  public UnqualifiedClassInstanceCreationExpression withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withClassOrInterface(hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate classOrInterface) {
    java.util.Objects.requireNonNull((classOrInterface));
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withArguments(java.util.List<hydra.ext.java.syntax.Expression> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withBody(hydra.util.Opt<hydra.ext.java.syntax.ClassBody> body) {
    java.util.Objects.requireNonNull((body));
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
}
