// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class EnumMemberDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.EnumMemberDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final hydra.ext.csharp.syntax.Identifier name;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.ConstantExpression> value;
  
  public EnumMemberDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, hydra.ext.csharp.syntax.Identifier name, hydra.util.Opt<hydra.ext.csharp.syntax.ConstantExpression> value) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((value));
    this.attributes = attributes;
    this.name = name;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumMemberDeclaration)) {
      return false;
    }
    EnumMemberDeclaration o = (EnumMemberDeclaration) (other);
    return attributes.equals(o.attributes) && name.equals(o.name) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * name.hashCode() + 5 * value.hashCode();
  }
  
  public EnumMemberDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new EnumMemberDeclaration(attributes, name, value);
  }
  
  public EnumMemberDeclaration withName(hydra.ext.csharp.syntax.Identifier name) {
    java.util.Objects.requireNonNull((name));
    return new EnumMemberDeclaration(attributes, name, value);
  }
  
  public EnumMemberDeclaration withValue(hydra.util.Opt<hydra.ext.csharp.syntax.ConstantExpression> value) {
    java.util.Objects.requireNonNull((value));
    return new EnumMemberDeclaration(attributes, name, value);
  }
}