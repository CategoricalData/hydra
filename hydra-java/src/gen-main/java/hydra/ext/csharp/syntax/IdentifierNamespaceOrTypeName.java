// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class IdentifierNamespaceOrTypeName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.IdentifierNamespaceOrTypeName");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> arguments;
  
  public IdentifierNamespaceOrTypeName (hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> arguments) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((arguments));
    this.identifier = identifier;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IdentifierNamespaceOrTypeName)) {
      return false;
    }
    IdentifierNamespaceOrTypeName o = (IdentifierNamespaceOrTypeName) (other);
    return identifier.equals(o.identifier) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * arguments.hashCode();
  }
  
  public IdentifierNamespaceOrTypeName withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new IdentifierNamespaceOrTypeName(identifier, arguments);
  }
  
  public IdentifierNamespaceOrTypeName withArguments(hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new IdentifierNamespaceOrTypeName(identifier, arguments);
  }
}