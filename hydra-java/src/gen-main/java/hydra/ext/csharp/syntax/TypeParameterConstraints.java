// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class TypeParameterConstraints implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.TypeParameterConstraints");
  
  public static final hydra.core.Name FIELD_NAME_PRIMARY = new hydra.core.Name("primary");
  
  public static final hydra.core.Name FIELD_NAME_SECONDARY = new hydra.core.Name("secondary");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRUCTOR = new hydra.core.Name("constructor");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.PrimaryConstraint> primary;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.SecondaryConstraints> secondary;
  
  public final Boolean constructor;
  
  public TypeParameterConstraints (hydra.util.Opt<hydra.ext.csharp.syntax.PrimaryConstraint> primary, hydra.util.Opt<hydra.ext.csharp.syntax.SecondaryConstraints> secondary, Boolean constructor) {
    java.util.Objects.requireNonNull((primary));
    java.util.Objects.requireNonNull((secondary));
    java.util.Objects.requireNonNull((constructor));
    this.primary = primary;
    this.secondary = secondary;
    this.constructor = constructor;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeParameterConstraints)) {
      return false;
    }
    TypeParameterConstraints o = (TypeParameterConstraints) (other);
    return primary.equals(o.primary) && secondary.equals(o.secondary) && constructor.equals(o.constructor);
  }
  
  @Override
  public int hashCode() {
    return 2 * primary.hashCode() + 3 * secondary.hashCode() + 5 * constructor.hashCode();
  }
  
  public TypeParameterConstraints withPrimary(hydra.util.Opt<hydra.ext.csharp.syntax.PrimaryConstraint> primary) {
    java.util.Objects.requireNonNull((primary));
    return new TypeParameterConstraints(primary, secondary, constructor);
  }
  
  public TypeParameterConstraints withSecondary(hydra.util.Opt<hydra.ext.csharp.syntax.SecondaryConstraints> secondary) {
    java.util.Objects.requireNonNull((secondary));
    return new TypeParameterConstraints(primary, secondary, constructor);
  }
  
  public TypeParameterConstraints withConstructor(Boolean constructor) {
    java.util.Objects.requireNonNull((constructor));
    return new TypeParameterConstraints(primary, secondary, constructor);
  }
}