// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class GlobalAttributeSection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.GlobalAttributeSection");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public final hydra.ext.csharp.syntax.Identifier target;
  
  public final hydra.ext.csharp.syntax.AttributeList attributes;
  
  public GlobalAttributeSection (hydra.ext.csharp.syntax.Identifier target, hydra.ext.csharp.syntax.AttributeList attributes) {
    java.util.Objects.requireNonNull((target));
    java.util.Objects.requireNonNull((attributes));
    this.target = target;
    this.attributes = attributes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GlobalAttributeSection)) {
      return false;
    }
    GlobalAttributeSection o = (GlobalAttributeSection) (other);
    return target.equals(o.target) && attributes.equals(o.attributes);
  }
  
  @Override
  public int hashCode() {
    return 2 * target.hashCode() + 3 * attributes.hashCode();
  }
  
  public GlobalAttributeSection withTarget(hydra.ext.csharp.syntax.Identifier target) {
    java.util.Objects.requireNonNull((target));
    return new GlobalAttributeSection(target, attributes);
  }
  
  public GlobalAttributeSection withAttributes(hydra.ext.csharp.syntax.AttributeList attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new GlobalAttributeSection(target, attributes);
  }
}