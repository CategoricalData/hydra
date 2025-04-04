// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class MethodReference_Primary implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.MethodReference_Primary");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_ARGUMENTS = new hydra.core.Name("typeArguments");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.ext.java.syntax.Primary primary;
  
  public final java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments;
  
  public final hydra.ext.java.syntax.Identifier identifier;
  
  public MethodReference_Primary (hydra.ext.java.syntax.Primary primary, java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments, hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((primary));
    java.util.Objects.requireNonNull((typeArguments));
    java.util.Objects.requireNonNull((identifier));
    this.primary = primary;
    this.typeArguments = typeArguments;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodReference_Primary)) {
      return false;
    }
    MethodReference_Primary o = (MethodReference_Primary) (other);
    return primary.equals(o.primary) && typeArguments.equals(o.typeArguments) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * typeArguments.hashCode() + 5 * identifier.hashCode();
  }
  
  public MethodReference_Primary withPrimary(hydra.ext.java.syntax.Primary primary) {
    java.util.Objects.requireNonNull((primary));
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
  
  public MethodReference_Primary withTypeArguments(java.util.List<hydra.ext.java.syntax.TypeArgument> typeArguments) {
    java.util.Objects.requireNonNull((typeArguments));
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
  
  public MethodReference_Primary withIdentifier(hydra.ext.java.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new MethodReference_Primary(primary, typeArguments, identifier);
  }
}