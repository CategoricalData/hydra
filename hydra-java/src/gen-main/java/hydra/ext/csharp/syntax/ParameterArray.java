// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class ParameterArray implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ParameterArray");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final hydra.ext.csharp.syntax.ArrayType type;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public ParameterArray (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, hydra.ext.csharp.syntax.ArrayType type, hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((identifier));
    this.attributes = attributes;
    this.type = type;
    this.identifier = identifier;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParameterArray)) {
      return false;
    }
    ParameterArray o = (ParameterArray) (other);
    return attributes.equals(o.attributes) && type.equals(o.type) && identifier.equals(o.identifier);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * type.hashCode() + 5 * identifier.hashCode();
  }
  
  public ParameterArray withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new ParameterArray(attributes, type, identifier);
  }
  
  public ParameterArray withType(hydra.ext.csharp.syntax.ArrayType type) {
    java.util.Objects.requireNonNull((type));
    return new ParameterArray(attributes, type, identifier);
  }
  
  public ParameterArray withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new ParameterArray(attributes, type, identifier);
  }
}