// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class LambdaParameter_Normal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.LambdaParameter.Normal");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_ID = new hydra.core.Name("id");
  
  public final java.util.List<hydra.langs.java.syntax.VariableModifier> modifiers;
  
  public final hydra.langs.java.syntax.LambdaParameterType type;
  
  public final hydra.langs.java.syntax.VariableDeclaratorId id;
  
  public LambdaParameter_Normal (java.util.List<hydra.langs.java.syntax.VariableModifier> modifiers, hydra.langs.java.syntax.LambdaParameterType type, hydra.langs.java.syntax.VariableDeclaratorId id) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((id));
    this.modifiers = modifiers;
    this.type = type;
    this.id = id;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LambdaParameter_Normal)) {
      return false;
    }
    LambdaParameter_Normal o = (LambdaParameter_Normal) (other);
    return modifiers.equals(o.modifiers) && type.equals(o.type) && id.equals(o.id);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * type.hashCode() + 5 * id.hashCode();
  }
  
  public LambdaParameter_Normal withModifiers(java.util.List<hydra.langs.java.syntax.VariableModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new LambdaParameter_Normal(modifiers, type, id);
  }
  
  public LambdaParameter_Normal withType(hydra.langs.java.syntax.LambdaParameterType type) {
    java.util.Objects.requireNonNull((type));
    return new LambdaParameter_Normal(modifiers, type, id);
  }
  
  public LambdaParameter_Normal withId(hydra.langs.java.syntax.VariableDeclaratorId id) {
    java.util.Objects.requireNonNull((id));
    return new LambdaParameter_Normal(modifiers, type, id);
  }
}