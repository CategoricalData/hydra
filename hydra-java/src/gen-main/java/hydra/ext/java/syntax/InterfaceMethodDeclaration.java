// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public class InterfaceMethodDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.InterfaceMethodDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_MODIFIERS = new hydra.core.Name("modifiers");
  
  public static final hydra.core.Name FIELD_NAME_HEADER = new hydra.core.Name("header");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final java.util.List<hydra.ext.java.syntax.InterfaceMethodModifier> modifiers;
  
  public final hydra.ext.java.syntax.MethodHeader header;
  
  public final hydra.ext.java.syntax.MethodBody body;
  
  public InterfaceMethodDeclaration (java.util.List<hydra.ext.java.syntax.InterfaceMethodModifier> modifiers, hydra.ext.java.syntax.MethodHeader header, hydra.ext.java.syntax.MethodBody body) {
    java.util.Objects.requireNonNull((modifiers));
    java.util.Objects.requireNonNull((header));
    java.util.Objects.requireNonNull((body));
    this.modifiers = modifiers;
    this.header = header;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InterfaceMethodDeclaration)) {
      return false;
    }
    InterfaceMethodDeclaration o = (InterfaceMethodDeclaration) (other);
    return modifiers.equals(o.modifiers) && header.equals(o.header) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * modifiers.hashCode() + 3 * header.hashCode() + 5 * body.hashCode();
  }
  
  public InterfaceMethodDeclaration withModifiers(java.util.List<hydra.ext.java.syntax.InterfaceMethodModifier> modifiers) {
    java.util.Objects.requireNonNull((modifiers));
    return new InterfaceMethodDeclaration(modifiers, header, body);
  }
  
  public InterfaceMethodDeclaration withHeader(hydra.ext.java.syntax.MethodHeader header) {
    java.util.Objects.requireNonNull((header));
    return new InterfaceMethodDeclaration(modifiers, header, body);
  }
  
  public InterfaceMethodDeclaration withBody(hydra.ext.java.syntax.MethodBody body) {
    java.util.Objects.requireNonNull((body));
    return new InterfaceMethodDeclaration(modifiers, header, body);
  }
}