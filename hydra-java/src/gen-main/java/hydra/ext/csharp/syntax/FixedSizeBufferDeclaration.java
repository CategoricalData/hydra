// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class FixedSizeBufferDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.FixedSizeBufferDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_ELEMENT_TYPE = new hydra.core.Name("elementType");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATORS = new hydra.core.Name("declarators");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.FixedSizeBufferModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.Type elementType;
  
  public final java.util.List<hydra.ext.csharp.syntax.FixedSizeBufferDeclarator> declarators;
  
  public FixedSizeBufferDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.FixedSizeBufferModifier> modifiers, hydra.ext.csharp.syntax.Type elementType, java.util.List<hydra.ext.csharp.syntax.FixedSizeBufferDeclarator> declarators) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((elementType));
    java.util.Objects.requireNonNull((declarators));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.elementType = elementType;
    this.declarators = declarators;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FixedSizeBufferDeclaration)) {
      return false;
    }
    FixedSizeBufferDeclaration o = (FixedSizeBufferDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && elementType.equals(o.elementType) && declarators.equals(o.declarators);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * elementType.hashCode() + 7 * declarators.hashCode();
  }
  
  public FixedSizeBufferDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new FixedSizeBufferDeclaration(attributes, modifiers, elementType, declarators);
  }
  
  public FixedSizeBufferDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.FixedSizeBufferModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new FixedSizeBufferDeclaration(attributes, modifiers, elementType, declarators);
  }
  
  public FixedSizeBufferDeclaration withElementType(hydra.ext.csharp.syntax.Type elementType) {
    java.util.Objects.requireNonNull((elementType));
    return new FixedSizeBufferDeclaration(attributes, modifiers, elementType, declarators);
  }
  
  public FixedSizeBufferDeclaration withDeclarators(java.util.List<hydra.ext.csharp.syntax.FixedSizeBufferDeclarator> declarators) {
    java.util.Objects.requireNonNull((declarators));
    return new FixedSizeBufferDeclaration(attributes, modifiers, elementType, declarators);
  }
}