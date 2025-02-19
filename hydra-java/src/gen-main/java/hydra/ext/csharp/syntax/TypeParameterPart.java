// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class TypeParameterPart implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.TypeParameterPart");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final hydra.ext.csharp.syntax.TypeParameter name;
  
  public TypeParameterPart (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, hydra.ext.csharp.syntax.TypeParameter name) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((name));
    this.attributes = attributes;
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TypeParameterPart)) {
      return false;
    }
    TypeParameterPart o = (TypeParameterPart) (other);
    return attributes.equals(o.attributes) && name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * name.hashCode();
  }
  
  public TypeParameterPart withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new TypeParameterPart(attributes, name);
  }
  
  public TypeParameterPart withName(hydra.ext.csharp.syntax.TypeParameter name) {
    java.util.Objects.requireNonNull((name));
    return new TypeParameterPart(attributes, name);
  }
}