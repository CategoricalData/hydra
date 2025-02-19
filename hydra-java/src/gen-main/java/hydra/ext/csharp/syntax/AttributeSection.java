// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class AttributeSection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AttributeSection");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.AttributeTarget> target;
  
  public final hydra.ext.csharp.syntax.AttributeList attributes;
  
  public AttributeSection (hydra.util.Opt<hydra.ext.csharp.syntax.AttributeTarget> target, hydra.ext.csharp.syntax.AttributeList attributes) {
    java.util.Objects.requireNonNull((target));
    java.util.Objects.requireNonNull((attributes));
    this.target = target;
    this.attributes = attributes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AttributeSection)) {
      return false;
    }
    AttributeSection o = (AttributeSection) (other);
    return target.equals(o.target) && attributes.equals(o.attributes);
  }
  
  @Override
  public int hashCode() {
    return 2 * target.hashCode() + 3 * attributes.hashCode();
  }
  
  public AttributeSection withTarget(hydra.util.Opt<hydra.ext.csharp.syntax.AttributeTarget> target) {
    java.util.Objects.requireNonNull((target));
    return new AttributeSection(target, attributes);
  }
  
  public AttributeSection withAttributes(hydra.ext.csharp.syntax.AttributeList attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new AttributeSection(target, attributes);
  }
}