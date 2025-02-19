// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class BaseAccessWithIdentifier implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BaseAccessWithIdentifier");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments;
  
  public BaseAccessWithIdentifier (hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((typeArguments));
    this.identifier = identifier;
    this.typeArguments = typeArguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BaseAccessWithIdentifier)) {
      return false;
    }
    BaseAccessWithIdentifier o = (BaseAccessWithIdentifier) (other);
    return identifier.equals(o.identifier) && typeArguments.equals(o.typeArguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * identifier.hashCode() + 3 * typeArguments.hashCode();
  }
  
  public BaseAccessWithIdentifier withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new BaseAccessWithIdentifier(identifier, typeArguments);
  }
  
  public BaseAccessWithIdentifier withTypeArguments(hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new BaseAccessWithIdentifier(identifier, typeArguments);
  }
}