// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class UnqualifiedClassInstanceCreationExpression implements Serializable, Comparable<UnqualifiedClassInstanceCreationExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_OR_INTERFACE = new hydra.core.Name("classOrInterface");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate classOrInterface;
  
  public final java.util.List<hydra.ext.java.syntax.Expression> arguments;
  
  public final hydra.util.Maybe<hydra.ext.java.syntax.ClassBody> body;
  
  public UnqualifiedClassInstanceCreationExpression (java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate classOrInterface, java.util.List<hydra.ext.java.syntax.Expression> arguments, hydra.util.Maybe<hydra.ext.java.syntax.ClassBody> body) {
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
    UnqualifiedClassInstanceCreationExpression o = (UnqualifiedClassInstanceCreationExpression) other;
    return java.util.Objects.equals(
      this.typeArguments,
      o.typeArguments) && java.util.Objects.equals(
      this.classOrInterface,
      o.classOrInterface) && java.util.Objects.equals(
      this.arguments,
      o.arguments) && java.util.Objects.equals(
      this.body,
      o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeArguments) + 3 * java.util.Objects.hashCode(classOrInterface) + 5 * java.util.Objects.hashCode(arguments) + 7 * java.util.Objects.hashCode(body);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnqualifiedClassInstanceCreationExpression other) {
    int cmp = 0;
    cmp = Integer.compare(
      typeArguments.hashCode(),
      other.typeArguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) classOrInterface).compareTo(other.classOrInterface);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      arguments.hashCode(),
      other.arguments.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      body.hashCode(),
      other.body.hashCode());
  }
  
  public UnqualifiedClassInstanceCreationExpression withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withClassOrInterface(hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate classOrInterface) {
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withArguments(java.util.List<hydra.ext.java.syntax.Expression> arguments) {
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
  
  public UnqualifiedClassInstanceCreationExpression withBody(hydra.util.Maybe<hydra.ext.java.syntax.ClassBody> body) {
    return new UnqualifiedClassInstanceCreationExpression(typeArguments, classOrInterface, arguments, body);
  }
}
