// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class MethodModifiers implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.MethodModifiers");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_PARTIAL = new hydra.core.Name("partial");
  
  public final java.util.List<hydra.ext.csharp.syntax.MethodModifier> modifiers;
  
  public final Boolean partial;
  
  public MethodModifiers (java.util.List<hydra.ext.csharp.syntax.MethodModifier> modifiers, Boolean partial) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((partial));
    this.modifiers = modifiers;
    this.partial = partial;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MethodModifiers)) {
      return false;
    }
    MethodModifiers o = (MethodModifiers) (other);
    return modifiers.equals(o.modifiers) && partial.equals(o.partial);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * partial.hashCode();
  }
  
  public MethodModifiers withModifiers(java.util.List<hydra.ext.csharp.syntax.MethodModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new MethodModifiers(modifiers, partial);
  }
  
  public MethodModifiers withPartial(Boolean partial) {
    java.util.Objects.requireNonNull((partial));
    return new MethodModifiers(modifiers, partial);
  }
}