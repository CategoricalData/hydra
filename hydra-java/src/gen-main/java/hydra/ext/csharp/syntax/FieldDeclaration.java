// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class FieldDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.FieldDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DECLARATORS = new hydra.core.Name("declarators");
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.FieldModifier> modifiers;
  
  public final hydra.ext.csharp.syntax.Type type;
  
  public final java.util.List<hydra.ext.csharp.syntax.VariableDeclarator> declarators;
  
  public FieldDeclaration (hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes, java.util.List<hydra.ext.csharp.syntax.FieldModifier> modifiers, hydra.ext.csharp.syntax.Type type, java.util.List<hydra.ext.csharp.syntax.VariableDeclarator> declarators) {
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((declarators));
    this.attributes = attributes;
    this.modifiers = modifiers;
    this.type = type;
    this.declarators = declarators;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldDeclaration)) {
      return false;
    }
    FieldDeclaration o = (FieldDeclaration) (other);
    return attributes.equals(o.attributes) && modifiers.equals(o.modifiers) && type.equals(o.type) && declarators.equals(o.declarators);
  }
  
  @Override
  public int hashCode() {
    return 2 * attributes.hashCode() + 3 * modifiers.hashCode() + 5 * type.hashCode() + 7 * declarators.hashCode();
  }
  
  public FieldDeclaration withAttributes(hydra.util.Opt<hydra.ext.csharp.syntax.Attributes> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new FieldDeclaration(attributes, modifiers, type, declarators);
  }
  
  public FieldDeclaration withModifiers(java.util.List<hydra.ext.csharp.syntax.FieldModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new FieldDeclaration(attributes, modifiers, type, declarators);
  }
  
  public FieldDeclaration withType(hydra.ext.csharp.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new FieldDeclaration(attributes, modifiers, type, declarators);
  }
  
  public FieldDeclaration withDeclarators(java.util.List<hydra.ext.csharp.syntax.VariableDeclarator> declarators) {
    java.util.Objects.requireNonNull((declarators));
    return new FieldDeclaration(attributes, modifiers, type, declarators);
  }
}