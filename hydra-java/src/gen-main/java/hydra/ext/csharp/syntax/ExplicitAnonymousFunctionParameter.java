// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ExplicitAnonymousFunctionParameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ExplicitAnonymousFunctionParameter");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIER = new hydra.core.Name("modifier");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.AnonymousFunctionParameterModifier> modifier;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public ExplicitAnonymousFunctionParameter (hydra.util.Opt<hydra.ext.csharp.syntax.AnonymousFunctionParameterModifier> modifier, hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((modifier));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    this.modifier = modifier;
    this.type = type;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExplicitAnonymousFunctionParameter)) {
      return false;
    }
    ExplicitAnonymousFunctionParameter o = (ExplicitAnonymousFunctionParameter) (other);
    return modifier.equals(o.modifier) && type.equals(o.type) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifier.hashCode() + 3 * type.hashCode() + 5 * identifier.hashCode();
  }
  
  public ExplicitAnonymousFunctionParameter withModifier(hydra.util.Opt<hydra.ext.csharp.syntax.AnonymousFunctionParameterModifier> modifier) {
    java.util.Objects.requireNonNull((modifier));
    return new ExplicitAnonymousFunctionParameter(modifier, type, identifier);
  }
  
  public ExplicitAnonymousFunctionParameter withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new ExplicitAnonymousFunctionParameter(modifier, type, identifier);
  }
  
  public ExplicitAnonymousFunctionParameter withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new ExplicitAnonymousFunctionParameter(modifier, type, identifier);
  }
}