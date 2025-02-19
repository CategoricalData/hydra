// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class SimpleName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SimpleName");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments;
  
  public SimpleName (hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((typeArguments));
    this.identifier = identifier;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SimpleName)) {
      return false;
    }
    SimpleName o = (SimpleName) (other);
    return identifier.equals(o.identifier) && typeArguments.equals(o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * typeArguments.hashCode();
  }
  
  public SimpleName withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new SimpleName(identifier, typeArguments);
  }
  
  public SimpleName withTypeArguments(hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new SimpleName(identifier, typeArguments);
  }
}