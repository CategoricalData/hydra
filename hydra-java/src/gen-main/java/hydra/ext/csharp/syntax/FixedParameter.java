// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class FixedParameter implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.FixedParameter");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIER = new hydra.core.Name("modifier");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_ARGUMENT = new hydra.core.Name("defaultArgument");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ParameterModifier> modifier;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Expression> defaultArgument;
  
  public FixedParameter (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, hydra.util.Opt<hydra.ext.csharp.syntax.ParameterModifier> modifier, hydra.ext.csharp.syntax.Type type, hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.Expression> defaultArgument) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifier));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((defaultArgument));
    this.attributes = attributes;
    this.modifier = modifier;
    this.type = type;
    this.identifier = identifier;
    this.defaultArgument = defaultArgument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixedParameter)) {
      return false;
    }
    FixedParameter o = (FixedParameter) (other);
    return attributes.equals(o.attributes) && modifier.equals(o.modifier) && type.equals(o.type) && identifier.equals(o.identifier) && defaultArgument.equals(o.defaultArgument);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifier.hashCode() + 5 * type.hashCode() + 7 * identifier.hashCode() + 11 * defaultArgument.hashCode();
  }
  
  public FixedParameter withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new FixedParameter(attributes, modifier, type, identifier, defaultArgument);
  }
  
  public FixedParameter withModifier(hydra.util.Opt<hydra.ext.csharp.syntax.ParameterModifier> modifier) {
    java.util.Objects.requireNonNull((modifier));
    return new FixedParameter(attributes, modifier, type, identifier, defaultArgument);
  }
  
  public FixedParameter withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new FixedParameter(attributes, modifier, type, identifier, defaultArgument);
  }
  
  public FixedParameter withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new FixedParameter(attributes, modifier, type, identifier, defaultArgument);
  }
  
  public FixedParameter withDefaultArgument(hydra.util.Opt<hydra.ext.csharp.syntax.Expression> defaultArgument) {
    java.util.Objects.requireNonNull((defaultArgument));
    return new FixedParameter(attributes, modifier, type, identifier, defaultArgument);
  }
}