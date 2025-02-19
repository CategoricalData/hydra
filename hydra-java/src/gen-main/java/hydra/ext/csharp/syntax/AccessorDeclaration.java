// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class AccessorDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AccessorDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIER = new hydra.core.Name("modifier");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.AccessorModifier> modifier;
  
  public final hydra.ext.csharp.syntax.AccessorBody body;
  
  public AccessorDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, hydra.util.Opt<hydra.ext.csharp.syntax.AccessorModifier> modifier, hydra.ext.csharp.syntax.AccessorBody body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifier));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.modifier = modifier;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AccessorDeclaration)) {
      return false;
    }
    AccessorDeclaration o = (AccessorDeclaration) (other);
    return attributes.equals(o.attributes) && modifier.equals(o.modifier) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifier.hashCode() + 5 * body.hashCode();
  }
  
  public AccessorDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new AccessorDeclaration(attributes, modifier, body);
  }
  
  public AccessorDeclaration withModifier(hydra.util.Opt<hydra.ext.csharp.syntax.AccessorModifier> modifier) {
    java.util.Objects.requireNonNull((modifier));
    return new AccessorDeclaration(attributes, modifier, body);
  }
  
  public AccessorDeclaration withBody(hydra.ext.csharp.syntax.AccessorBody body) {
    java.util.Objects.requireNonNull((body));
    return new AccessorDeclaration(attributes, modifier, body);
  }
}