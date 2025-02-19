// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class AddRemoveAccessorDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AddRemoveAccessorDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final hydra.ext.csharp.syntax.Block body;
  
  public AddRemoveAccessorDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, hydra.ext.csharp.syntax.Block body) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((body));
    this.attributes = attributes;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AddRemoveAccessorDeclaration)) {
      return false;
    }
    AddRemoveAccessorDeclaration o = (AddRemoveAccessorDeclaration) (other);
    return attributes.equals(o.attributes) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * body.hashCode();
  }
  
  public AddRemoveAccessorDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new AddRemoveAccessorDeclaration(attributes, body);
  }
  
  public AddRemoveAccessorDeclaration withBody(hydra.ext.csharp.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new AddRemoveAccessorDeclaration(attributes, body);
  }
}